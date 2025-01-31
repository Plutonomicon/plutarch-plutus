{ pkgs, lib, ... }:
{ cabalProject
, prologue ? "= Combined Haddock"
, targetPackages
}:
let

  # Haskell packages to make documentation for. Only those with a "doc" output will be used.
  # Note: we do not provide arbitrary additional Haddock options, as these would not be
  # applied consistently, since we're reusing the already built Haddock for the packages.
  hsPkgs =
    lib.attrValues (
      pkgs.haskell-nix.haskellLib.collectComponents' "library" (
        pkgs.haskell-nix.haskellLib.selectProjectPackages cabalProject.hsPkgs
        //
        (
          lib.filterAttrs
            (name: _: lib.any (x: builtins.match "(${x})-.*$" name == [ x ]) targetPackages)
            cabalProject.hsPkgs
        )
      )
    );


  hsPkgs-docs = map (x: x.doc) (lib.filter (x: x ? doc) hsPkgs);

  prologueFile =
    pkgs.writeTextFile {
      name = "prologue";
      text = prologue;
    };

  the-mighty-command = ''
    hsdocsRec="$(cat graph* | grep -F /nix/store | sort | uniq)"

    # Merge all the docs from the packages and their doc dependencies.
    # We don't use symlinkJoin because:
    # - We are going to want to redistribute this, so we don't want any symlinks.
    # - We want to be selective about what we copy (we don't need the hydra
    #   tarballs from the other packages, for example.
    mkdir -p "$out/share/doc"

    for pkg in $hsdocsRec; do
      if [ -d "$pkg/share/doc/ghc" ]; then
        echo "Skipping GHC :'("
      elif [ -d "$pkg/share/doc" ]; then
        cp -r "$pkg/share/doc/." "$out/share/doc"
      fi
    done

    # We're going to sed all the files so they'd better be writable!
    chmod -R +w $out/share/doc

    # We're now going to rewrite all the pre-generated Haddock HTML output
    # so that links point to the appropriate place within our combined output,
    # rather than into the store.
    root=$out/share/doc
    for f in $(find $out -name "*.html"); do
      # Replace all links to the docs we're processing with relative links
      # to the root of the doc directory we're creating - the rest of the link is
      # the same.
      # Also, it's not a a file:// link now because it's a relative URL instead
      # of an absolute one.
      relpath=$(realpath --relative-to=$(dirname $f) --no-symlinks $root)
      pkgsRegex="${"file:///nix/store/[^/]*/share/doc"}"
      sed -i -r "s,$pkgsRegex,$relpath,g" "$f"
    done

    # Move to the docdir. We do this so that we can give relative docpaths to
    # Haddock so it will generate relative (relocatable) links in the index.
    cd $out/share/doc
    # Non-recursively collect all the interface files and their docpaths
    # (in this case we can just use the enclosing directory).
    interfaceOpts=()
    for pkg in ${lib.concatStringsSep " " hsPkgs-docs}; do
      pushd $pkg/share/doc
      for interfaceFile in $(find . -name "*.haddock"); do
        # this is '$PACKAGE/html'
        docdir=$(dirname $interfaceFile)
        interfaceOpts+=("--read-interface=$docdir,$interfaceFile")
      done
      popd
    done

    # Generate the contents and index
    ${cabalProject.pkg-set.config.ghc.package}/bin/haddock \
      --gen-contents \
      --gen-index \
      --quickjump \
      --prologue ${prologueFile} \
      "''${interfaceOpts[@]}"

    # TODO: remove patch when haddock > 2.24.0
    # patch quick-jump.css to fix scrolling in search for chromium
    for f in $(find $out -name "quick-jump.css"); do
      sed -i -r "s,^\#search-results \{,\#search-results \{ max-height:80%;overflow-y:scroll;," "$f"
    done

    # Following: https://github.com/input-output-hk/ouroboros-network/blob/2068d091bc7dcd3f4538fb76f1b598f219d1e0c8/scripts/haddocs.sh#L87 # editorconfig-checker-disable-line
    # Assemble a toplevel `doc-index.json` from package level ones.
    shopt -s globstar
    echo "[]" > "doc-index.json"
    for file in $(ls **/doc-index.json); do
      project=$(dirname $file);
      ${pkgs.jq}/bin/jq -s ".[0] + [.[1][] | (. + {link: (\"$project/\" + .link)}) ]" \
        "doc-index.json" "$file" > doc-index.tmp.json
      mv doc-index.tmp.json "doc-index.json"
    done

    echo "Done Combining Haddock"
  '';


  command-args = {
    buildInputs = [ hsPkgs-docs ];

    # For each package in hsdocs, this will create a file `graph-N` (where N is the index in the list)
    # which contains information about which nix paths are referenced by the package. This will allow
    # us to resolve hyperlinks to haddocks elsewhere in the store.
    #
    # See also https://nixos.org/manual/nix/stable/expressions/advanced-attributes.html#adv-attr-exportReferencesGraph # editorconfig-checker-disable-line
    exportReferencesGraph =
      lib.concatLists
        (lib.imap0 (i: pkg: [ "graph-${toString i}" pkg ]) hsPkgs-docs);
  };

  combined-haddock = pkgs.runCommand "combine-haddock" command-args the-mighty-command;
in
combined-haddock
