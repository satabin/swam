pw=wasi/test/resources

echo "Read args"
mill cli.run $pw/read-args.wasm t1  retyty erte

echo "Merge sort"
mill cli.run $pw/merge_sort.wasm

echo "Posix"
mill cli.run $pw/posix.wasm m1.txt 1.txt

echo "Bitwise"
mill cli.run $pw/bitwise.wasm

echo "Read dir"
mill cli.run $pw/readdir.wasm -d .

echo "mkdir"
mill cli.run $pw/mkdir.wasm

#wasi/test/resources/read-args.wasm t1  retyty erte
#