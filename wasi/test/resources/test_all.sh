pw=wasi/test/resources

mill cli.run $pw/read-args.wasm t1  retyty erte
mill cli.run $pw/merge_sort.wasm
mill cli.run $pw/posix.wasm m1.txt m2.txt