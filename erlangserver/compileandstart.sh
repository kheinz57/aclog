#!/bin/sh
erlc aclog.erl
erl -noshell -detached -run aclog main &

