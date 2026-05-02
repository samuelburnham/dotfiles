---Snacks.nvim terminal provider for Claude Code — patched to NOT force insert
---mode. Vendored from claudecode.nvim/lua/claudecode/terminal/snacks.lua.
---
---Patches from upstream:
---  1. build_opts: start_insert and auto_insert hardcoded to false (was = focus)
---  2. Three `vim.cmd("startinsert")` blocks removed from open() and focus_toggle()
---  3. Newline keymap rebuilt: bound to <NL>, <S-CR>, and <C-CR> so it fires
---     on whichever encoding the host terminal uses for Shift-Enter (Ghostty
---     sends LF; kitty-keyboard terminals send CSI-u). Action goes through
---     chansend instead of two deferred feedkeys calls — single PTY write,
---     no race.
---
---Sync with upstream if claudecode.nvim bumps versions and you see startinsert
---regressing. Check the file at:
---   <plugin>/lua/claudecode/terminal/snacks.lua
---@module 'claudecode-no-insert'

local M = {}

local snacks_available, Snacks = pcall(require, "snacks")
local utils = require("claudecode.utils")
local terminal = nil

--- @return boolean
local function is_available()
  return snacks_available and Snacks and Snacks.terminal ~= nil
end

---Setup event handlers for terminal instance
local function setup_terminal_events(term_instance, config)
  local logger = require("claudecode.logger")

  if config.auto_close then
    term_instance:on("TermClose", function()
      if vim.v.event.status ~= 0 then
        logger.error(
          "terminal",
          "Claude exited with code " .. vim.v.event.status .. ".\nCheck for any errors."
        )
      end
      terminal = nil
      vim.schedule(function()
        term_instance:close({ buf = true })
        vim.cmd.checktime()
      end)
    end, { buf = true })
  end

  term_instance:on("BufWipeout", function()
    logger.debug("terminal", "Terminal buffer wiped")
    terminal = nil
  end, { buf = true })
end

---Send Claude Code's literal-newline escape (`\` then Enter) directly to the
---terminal job. Bound under every encoding terminals use for Shift-Enter:
---<NL> (Ghostty's literal LF), <S-CR> and <C-CR> (CSI-u / kitty-keyboard).
---Plain xterm / GNOME Terminal collapse Shift-Enter to bare CR — no fix
---possible from inside nvim there; switch terminals or use a different chord.
local function send_newline()
  local job = vim.b.terminal_job_id
  if job then
    vim.fn.chansend(job, "\\\r")
  end
end

---Builds Snacks terminal options — PATCH: start_insert and auto_insert forced to false
local function build_opts(config, env_table, focus)
  focus = utils.normalize_focus(focus)
  return {
    env = env_table,
    cwd = config.cwd,
    start_insert = false, -- PATCH: was `focus`
    auto_insert = false,  -- PATCH: was `focus`
    auto_close = false,
    win = vim.tbl_deep_extend("force", {
      position = config.split_side,
      width = config.split_width_percentage,
      height = 0,
      relative = "editor",
      keys = {
        claude_newline_nl = { "<NL>",   send_newline, mode = "t", desc = "New line" },
        claude_newline_sc = { "<S-CR>", send_newline, mode = "t", desc = "New line" },
        claude_newline_cc = { "<C-CR>", send_newline, mode = "t", desc = "New line" },
        -- NOTE: snacks' "double escape to normal mode" default is left in
        -- place here. A buffer-local <Esc> keymap set via an autocmd in
        -- nvim.nix overrides it at BufEnter time, giving single-press exit.
      },
    }, config.snacks_win_opts or {}),
  }
end

function M.setup()
  -- No specific setup needed
end

function M.open(cmd_string, env_table, config, focus)
  if not is_available() then
    vim.notify(
      "Snacks.nvim terminal provider selected but Snacks.terminal not available.",
      vim.log.levels.ERROR
    )
    return
  end

  focus = utils.normalize_focus(focus)

  if terminal and terminal:buf_valid() then
    if not terminal.win or not vim.api.nvim_win_is_valid(terminal.win) then
      terminal:toggle()
      if focus then
        terminal:focus()
        -- PATCH: removed vim.cmd("startinsert") block
      end
    else
      if focus then
        terminal:focus()
        -- PATCH: removed vim.cmd("startinsert") block
      end
    end
    return
  end

  local opts = build_opts(config, env_table, focus)
  local term_instance = Snacks.terminal.open(cmd_string, opts)
  if term_instance and term_instance:buf_valid() then
    setup_terminal_events(term_instance, config)
    terminal = term_instance
  else
    terminal = nil
    local logger = require("claudecode.logger")
    local error_details = {}
    if not term_instance then
      table.insert(error_details, "Snacks.terminal.open() returned nil")
    elseif not term_instance:buf_valid() then
      table.insert(error_details, "terminal instance is invalid")
      if term_instance.buf and not vim.api.nvim_buf_is_valid(term_instance.buf) then
        table.insert(error_details, "buffer is invalid")
      end
      if term_instance.win and not vim.api.nvim_win_is_valid(term_instance.win) then
        table.insert(error_details, "window is invalid")
      end
    end

    local context = string.format("cmd='%s', opts=%s", cmd_string, vim.inspect(opts))
    local error_msg = string.format(
      "Failed to open Claude terminal using Snacks. Details: %s. Context: %s",
      table.concat(error_details, ", "),
      context
    )
    vim.notify(error_msg, vim.log.levels.ERROR)
    logger.debug("terminal", error_msg)
  end
end

function M.close()
  if not is_available() then
    return
  end
  if terminal and terminal:buf_valid() then
    terminal:close()
  end
end

function M.simple_toggle(cmd_string, env_table, config)
  if not is_available() then
    vim.notify(
      "Snacks.nvim terminal provider selected but Snacks.terminal not available.",
      vim.log.levels.ERROR
    )
    return
  end

  local logger = require("claudecode.logger")

  if terminal and terminal:buf_valid() and terminal:win_valid() then
    logger.debug("terminal", "Simple toggle: hiding visible terminal")
    terminal:toggle()
  elseif terminal and terminal:buf_valid() and not terminal:win_valid() then
    logger.debug("terminal", "Simple toggle: showing hidden terminal")
    terminal:toggle()
  else
    logger.debug("terminal", "Simple toggle: creating new terminal")
    M.open(cmd_string, env_table, config)
  end
end

function M.focus_toggle(cmd_string, env_table, config)
  if not is_available() then
    vim.notify(
      "Snacks.nvim terminal provider selected but Snacks.terminal not available.",
      vim.log.levels.ERROR
    )
    return
  end

  local logger = require("claudecode.logger")

  if terminal and terminal:buf_valid() and not terminal:win_valid() then
    logger.debug("terminal", "Focus toggle: showing hidden terminal")
    terminal:toggle()
  elseif terminal and terminal:buf_valid() and terminal:win_valid() then
    local claude_term_neovim_win_id = terminal.win
    local current_neovim_win_id = vim.api.nvim_get_current_win()

    if claude_term_neovim_win_id == current_neovim_win_id then
      logger.debug("terminal", "Focus toggle: hiding terminal (currently focused)")
      terminal:toggle()
    else
      logger.debug("terminal", "Focus toggle: focusing terminal")
      vim.api.nvim_set_current_win(claude_term_neovim_win_id)
      -- PATCH: removed vim.cmd("startinsert") block
    end
  else
    logger.debug("terminal", "Focus toggle: creating new terminal")
    M.open(cmd_string, env_table, config)
  end
end

function M.toggle(cmd_string, env_table, config)
  M.simple_toggle(cmd_string, env_table, config)
end

function M.get_active_bufnr()
  if terminal and terminal:buf_valid() and terminal.buf then
    if vim.api.nvim_buf_is_valid(terminal.buf) then
      return terminal.buf
    end
  end
  return nil
end

function M.is_available()
  return is_available()
end

return M
