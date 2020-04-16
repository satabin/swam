pw=wasi/test/resources

echo "Read args"
mill cli.run $pw/read-args.wasm t1  retyty erte

echo "Merge sort"
mill cli.run $pw/merge_sort.wasm

echo "Posix"
mill cli.run -d ./wasi/test/resources $pw/posix.wasm test_all.sh copy.txt

echo "Bitwise"
mill cli.run $pw/bitwise.wasm

echo "Read dir"
mill cli.run $pw/readdir.wasm -d .

echo "mkdir"
mill cli.run $pw/mkdir.wasm


echo "URL decoding"
mill cli.run $pw/URL_decoding.wasm


echo "no such"
mill cli.run -m main $pw/nosuchcon_2013_whitebox_noenc.wasm 12312312313123


echo "a"
mill cli.run $pw/a.wasm 12312312313123
#wasi/test/resources/read-args.wasm t1  retyty erte
#