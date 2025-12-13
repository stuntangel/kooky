# Kooky overlays
{ inputs, ... }:

final: prev: {
  # Add kooky configuration package to pkgs
  kooky = final.callPackage ../.. { };

  # Add kooky Emacs package (Emacs with all dependencies)
  kookyEmacs = final.callPackage ../../emacs.nix { devMode = false; };

  # Custom Emacs package overrides can go here
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (efinal: esuper: {
    # Example: Override a package
    page-view = esuper.trivialBuild rec {
      pname = "page_view";
      version = "2025-12-12";
      src = final.fetchFromGitHub {
        owner = "bradmont";
        repo = "page-view";
        rev = "eee218c6f314b40aa15ce88d1b31a2686496976e";
        hash = "sha256-B/zdXuD9nEtYFvOKRu2ejjBiKvnjWW49QfdmvXAYUik=";
      };
      propagatedUserEnvPkgs = [
        esuper.olivetti
      ];
      buildInputs = propagatedUserEnvPkgs;
    };
    # magit = esuper.magit.overrideAttrs (old: {
    #   # customizations
    # });
  });
}
