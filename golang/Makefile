rpc: clean-rpc rpc-client rpc-server

rpc-client:
	go build -o bin/rpc/client github.com/ldcc/rpc/examples/rpc/client
rpc-server:
	go build -o bin/rpc/server github.com/ldcc/rpc/examples/rpc/server

json2: clean-json2 json2-client json2-server

json2-client:
	go build -o bin/json2/client github.com/ldcc/rpc/examples/json2/client
json2-server:
	go build -o bin/json2/server github.com/ldcc/rpc/examples/json2/server

json2-array: clean-json2-array json2-array-client json2-array-server

json2-array-client:
	go build -o bin/json2-array/client github.com/ldcc/rpc/examples/json2-array/client
json2-array-server:
	go build -o bin/json2-array/server github.com/ldcc/rpc/examples/json2-array/server

clean: clean-rpc clean-json2 clean-json2-array

clean-rpc:
	rm -rf bin/rpc
clean-json2:
	rm -rf bin/json2
clean-json2-array:
	rm -rf bin/json2-array

.PHONY: clean clean-rpc clean-json2 clean-json2-array