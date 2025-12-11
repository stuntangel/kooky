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
    # magit = esuper.magit.overrideAttrs (old: {
    #   # customizations
    # });
  });
}
