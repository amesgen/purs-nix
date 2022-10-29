with builtins;
{ switches
, l
, p
, ps-pkgs
, purs
, ...
}:
  let
    buckets =
      set-buckets 1 ps-pkgs
      // set-buckets 2 (get-namespace "ursi")
      // { event = 2;
           task = 2;
         };

    get-namespace = ns: l.filterAttrs (n: _: l.hasPrefix "${ns}." n) ps-pkgs;
    set-buckets = n: mapAttrs (_: _: n);

    dependencies =
      foldl'
        (acc: { name, value }:
           let
             bucket = toString buckets.${name};
           in
           acc
           // { ${bucket} =
                  if acc?${bucket}
                  then acc.${bucket} ++ [ value ]
                  else [ value ];
              }
        )
        {}
        (l.mapAttrsToList l.nameValuePair ps-pkgs);
  in
  # to truly test this you need to manually wipe the caches in ~/.cache/nix
  l.foldl'
    (acc: { name, value }:
       let
         ps = purs { dependencies = value; };
         test-name = "compiled packages bucket ${name}";
       in

       acc
       // l.optionalAttrs switches.packages-compile
            { ${test-name} =
                let
                  command =
                    ps.command
                      { output = "$out";
                        srcs = [];
                      }
                    + "/bin/purs-nix";
                in
                p.runCommand test-name {} "${command} compile";
            }
    )
    {}
    (l.mapAttrsToList l.nameValuePair dependencies)
