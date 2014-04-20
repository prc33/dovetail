#!/bin/sh

sed -r '/select i1/ {
N
N
s/(select i1 )(%[0-9]+)(, %msg\.slow_cell\* %[0-9]+, %msg\.slow_cell\* null\s+%fcheck\.[0-9]+\.\S+ = icmp eq %msg\.slow_cell\* \S+, null\s+br i1 )(%\S+)(, label \S+)(, label \S+)/\1\2\3\2\6\5/g}' < $1 > $2

