#!/bin/bash
curl http://127.0.0.1:8080 \
	-i -k \
	--header "Content-Type: application/json" \
	-d '{"foo":{"bar": "Hello world"}}'
