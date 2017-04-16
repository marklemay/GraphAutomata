#!/bin/bash
set -x

input_dir=$1
cd $input_dir
output_dir=$2

for i in `ls $input_dir`; do
    neo4j stop
    sleep 10
    data_set_dir=$input_dir/$i/"spade.graph_db/"
    neo_dir="/usr/local/Cellar/neo4j/3.1.3/libexec/data/databases/graph.db/"
    graph_auto_dir="/Users/wajih/projects/GraphAutomata/"
    rm -rf $neo_dir/*
    cp -R $data_set_dir/* $neo_dir/
    neo4j start
    sleep 20
    cd $graph_auto_dir
    mvn test -Dtest=TestNeoGraphs#learnNeo4j
    mkdir -p $output_dir/$i/
    cp stats.csv $output_dir/$i/
    cp time_cost.csv $output_dir/$i/
    cd -
done
