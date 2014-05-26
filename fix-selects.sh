#!/bin/sh

sed -r '/select i1/ {
N
N
s/(select i1 )(%[0-9]+)(, %\S+\* %\S+, %\S+\* null, \!dbg \![0-9]+\s+%fcheck\.[0-9]+\.\S+ = icmp eq %\S*\* \S+, null\s+br i1 )(%\S+)(, label \S+)(, label \S+)/\1\2\3\2\6\5/g}' < $1 > $2

