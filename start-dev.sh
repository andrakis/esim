#!/bin/sh

exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname esim \
    -s esim \
    +K true \
    +A 5 \
    -config config/esim.config
