# SelectQAOA - Quantum Regression Test Case Selection
This repository contains all the necessary resources to reproduce the results of the 
SelectQAOA method.

## Dataset Files

The "./datasets" folder contains:

- "sir_programs"
- "boot_qa_datasets"

### SIR Programs

The "./datasets/sir_programs" folder contains, for each SIR program considered by this project, 
all the files needed to gather statement coverage, execution costs, and past fault 
coverage information.

Let's take the "flex" program as an example.

Inside the folder "./datasets/sir_programs/flex" the file "fault-matrix.txt" contains many rows 
as flex's test cases. Each row is divided by (in this case) 5 columns, representing 5 
different versions of the program. Each cell (i,j) contains a binary (0 or 1) value, 
representing the ability of the i-th test case to spot a fault in the j-th version 
of the program. This configuration is called fault matrix and is the resource needed 
to gain historical fault coverage information.

The folder "./datasets/sir_programs/json_flex" contains a folder for each test case. The 
generic "ti" folder (i.e. the folder related to the i-th test case of flex) contains 
the "flexi.gcov.json" file. This JSON file is fundamental to recovering statement 
coverage and execution costs information since it tells us for each basic block of 
the system under test if the i-th test case did execute/cover it and how many times
the i-th test case executed it. This way, we can derive the total statement coverage 
and execution cost of a single test case.

### BootQA Datasets

The "./datasets/boot_qa_datasets" folder contains, for each dataset used to compare SelectQA and 
BootQA, all the files needed to gather execution costs and failure rate information.

Let's use gsdtsr as an example.

The folder "./datasets/boot_qa_datasets" contains for example the file "gdtsr.csv" dataset file. This 
file contains, except for the first, many rows as test cases. Each row is then 
divided by three columns: id, time, and rate.

## Source Code Files

### MOQ-Pipeline.ipynb

This file contains three main algorithms:

- The three-objectives version of SelectQAOA;
- The two-objectives version of SelectQAOA.

This file implements all the pipelines necessary to run the algorithms, from the dataset 
information gathering, to algorithm execution and finally to empirical comparisons.

This file could be divided in two main sections.

#### SelectQAOA vs SelectQA and Classical Algorithms

In the first section, the pipelines start from the analysis of SIR programs and end
with empirical comparisons between SelectQAOA, SelectQA and the classical algorithms.
Please, note that, while the dataset analysis and algorithms execution are generalized 
and automatically executable, the empirical evaluation part must be manually 
configured each time we change the target program. Also, once the target program has been 
manually changed, be sure to manually configure the different frontiers to correctly 
build the reference one.

#### SelectQAOA vs SelectQA and BootQA

The second section contains the pipeline to read the datasets and run SelectQAOA. This 
section is completely generalized and automated, so there is no need for manual 
configurations.

The files that contain the execution of BootQA and the empirical evaluations between 
SelectQA and BootQA are different and its description follows.

### stats.py

This file, contained in the root folder, executes the empirical comparisons between SelectQAOA, SelectQA and BootQA, and it is not 
necessary to configure it. The final results are stored in ".results/stats_results.csv" file.

## Results Files

The "./results" folder contains all the results obtained by SelectQAOA, DIV-GA, Additional Greedy, SelectQA and BootQA after all the experiment executions.
These files are needed to make all the empirical evaluations and comparisons between 
the three methods. The ".results/stats_results.csv" file contains the results of the statistical comparisons between SelectQAOA, SelectQA, and BootQA.
