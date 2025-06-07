{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    streamdeck = {
      url = "github:ners/streamdeck-haskell";
      flake = false;
    };
    hix = {
      url = "github:tek/hix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: inputs.hix (import ./hix.nix inputs);
}
