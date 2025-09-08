# Largely inspired by https://github.com/Misterio77/nix-config/blob/36f76f9a4e6dd45c692755858a248c26883184f5/home/gabriel/features/desktop/common/firefox.nix
{
  pkgs,
  lib,
  ...
}:
{
  programs.firefox = {
    enable = true;
    policies = {
      DisplayBookmarksToolbar = "always";
    };

    profiles.sam = {
      search = {
        force = true;
	default = "ddg";
	order = [ "ddg" "google" ];
      };
      # about:config settings
      settings = {
        "browser.startup.homepage" = "about:home";

        # Disable irritating first-run stuff
        "browser.disableResetPrompt" = true;
        "browser.download.panel.shown" = true;
        "browser.feeds.showFirstRunUI" = false;
        "browser.messaging-system.whatsNewPanel.enabled" = false;
        "browser.rights.3.shown" = true;
        "browser.shell.checkDefaultBrowser" = false;
        "browser.shell.defaultBrowserCheckCount" = 1;
        "browser.startup.homepage_override.mstone" = "ignore";
        "browser.uitour.enabled" = false;
        "startup.homepage_override_url" = "";
        "trailhead.firstrun.didSeeAboutWelcome" = true;
        "browser.bookmarks.restore_default_bookmarks" = false;
	# TODO: Prevent the "Import bookmarks" button from showing up in the toolbar
        "browser.bookmarks.addedImportButton" = true;

      	# Disable about:config warning
      	"browser.aboutConfig.showWarning" = false;
	"browser.tabs.loadBookmarksInTabs" = true;
        # Disable save passwords and autofill
      	"signon.rememberSignons" = false;
      	"extensions.formautofill.addresses.enabled" = false;
      	"extensions.formautofill.creditCards.enabled" = false;

        # Don't ask for download dir
        "browser.download.useDownloadDir" = false; # Doesn't seem to stick after login

        # Disable crappy home activity stream page
        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts" = false;
        "browser.newtabpage.blocked" = lib.genAttrs [
          # Youtube
          "26UbzFJ7qT9/4DhodHKA1Q=="
          # Facebook
          "4gPpjkxgZzXPVtuEoAL9Ig=="
          # Wikipedia
          "eV8/WsSLxHadrTL1gAxhug=="
          # Reddit
          "gLv0ja2RYVgxKdp0I5qwvA=="
          # Amazon
          "K00ILysCaEq8+bEqV/3nuw=="
          # Twitter
          "T9nJot5PurhJSy8n038xGA=="
        ] (_: 1);

        # Harden
	# These don't seem to stick after login
        "privacy.trackingprotection.enabled" = true;
        "dom.security.https_only_mode" = true; 

	# TODO: Customize layout of top bar and extensions
      };
    };
  };

  # Set Firefox as the default web browser
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = [ "firefox.desktop" ];
      "text/xml" = [ "firefox.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "application/pdf" = [ "firefox.desktop" ];
    };
  };

  #environment.sessionVariables.DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
}
