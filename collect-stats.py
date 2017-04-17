#!/usr/bin/python3
import sys
import argparse
import os

CWD = os.path.dirname(os.path.abspath(__file__))
arg_parser = argparse.ArgumentParser()

arg_parser.add_argument('input_dir', default=None,
                        help='s')
arg_parser.add_argument('output_dir', default=None,
                        help='g')

def parseClArgs():
    cl_args = arg_parser.parse_args()
    input_dir = CWD + '/' + os.path.relpath(cl_args.input_dir, CWD)
    output_dir = CWD + '/' + os.path.relpath(cl_args.output_dir, CWD)
    return input_dir,output_dir

def updateStats(mapp, val, count):
    mapp['total'] = mapp['total'] + val
    mapp['min'] =  val if mapp['min'] == 0 or val < mapp['min'] else mapp['min']
    mapp['max'] =  val if mapp['max'] == 0 or val > mapp['max'] else mapp['max']
    mapp['avg'] =  float(mapp['total']) / count if mapp['total'] != 0 else 0

def makeMacro(key, val):
    return '\DefMacro{' + key + '}{' + "{:,}".format(val) + '}'
    # return '\DefMacro{' + key + '}{' + str(round(val,3)) + '}'

def makeGivenMacros(stats):
    macros = []
    for bench,stat in stats.items():
        for key,value in stat.items():
            name = '-'.join([str(bench),str(key)])
            macros.append(makeMacro(name,value))

    return macros
    
def makeStatMacros(input_file, suffix, bench_name):
    keys = ['total', 'avg', 'min', 'max']
    state_map = dict([(key, 0.0) for key in keys])
    count = 1
    macros=[]
    with open(input_file) as fil:
        for line in fil:
            num = int(line.strip())
            updateStats(state_map,num,count)
            count+=1
    
    for key,value in state_map.items():
        name = '_'.join([str(key),str(suffix),str(bench_name)])
        macros.append(makeMacro(name,value))
    
    return macros
            
def writeMacrosToFile(macros, macros_file):
    with open(macros_file, 'w') as fil:
        for macro in macros:
            fil.write(macro + '\n')
def openReadVals(bench_name,input_path):
    comp_dir=input_path + "/" + bench_name
    all_stats = {}
    time_stats = {}
    
    with open(comp_dir + "/stats.csv") as fil:
        for l in fil:
            tokens = l.split(',')
            try:
                all_stats[tokens[0].strip()] = int(float(tokens[1].strip()))
            except:
                all_stats[tokens[0].strip()] = -1

    with open(comp_dir + "/time_cost.csv") as fil:
        for l in fil:
            tokens = l.split(',')
            time_stats[tokens[0].strip()] = int(float(tokens[1].strip()))
    
    return all_stats, time_stats
                
def main():
    input_dir, output_dir = parseClArgs()
    bench_names=["ftp"]
    stats = {}
    stats_time = {}
    macros = []
    for bench_name in bench_names:
        stats[bench_name],stats_time[bench_name] = openReadVals(bench_name,input_dir)
    print(stats)

    macros.extend(makeGivenMacros(stats))
    
    # macros.extend()
    # macros.extend(makeStatMacros(data_with_proxy,bench_name))
    for line in macros:
        print(line)
    file_name=output_dir+"/space_overhead_macros.tex"
    writeMacrosToFile(macros,file_name)
    
if __name__ == '__main__':
    main()
