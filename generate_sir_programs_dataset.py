import json

#this cell contains all variable definitions that will be useful throughout the entire project
sir_programs = ["flex","grep","gzip","sed"]
sir_programs_tests_number = {"flex":567,"grep":806,"gzip":214,"sed":360}
sir_programs_end_lines = {"flex":14192,"grep":13281,"gzip":6701,"sed":7118}
alpha = 0.5


# let's make a function to read the fault matrices
# IMPORTANT: all the fault-matrix files must be renamed as "fault-matrix".txt and must be written using the same standard used by the files of this project (i-th line->e0e1e2e3..., where ej is 0 if the i-th test found a bug when launched on the j-th version)
def get_fault_list(program_name: str):
    """This function opens the fault-matrix file of a sir programs and makes a list of binary values for each test case
    of that program to indicate whether a test case found or not a bug in at least one of the available versions
    of the program"""
    # open the fault-matrix file of the desired SIR program
    program_file = open("datasets/sir_programs/" + program_name + "/fault-matrix.txt")
    lines = program_file.readlines()

    # we need a list which elements represent test cases, the i-th element is 1 if the i-th test case
    # discovered a fault in the past, 0 otherwise
    faults_test_by_test = list()

    i = 0
    for line in lines:
        if "1" in line:
            faults_test_by_test.append(1)
        else:
            faults_test_by_test.append(0)
        i += 1

    program_file.close()

    return faults_test_by_test


# the next function is able to research into the json coverage information file of each test case
# of each sir program to gather information about the single test cases costs and coverage
def cost_and_coverage_information_gathering(program_name: str):
    """The aim of this function is to obtain a dictionary that for each test case of a program indicates its cost,
    and a dictionary that, for each code line of the program to test, makes a list of all the test cases that
    run that line"""
    test_case_execution_cost = 0

    execution_cost_test_by_test = dict()
    executed_lines_test_by_test = dict()
    test_coverage_line_by_line = dict()
    total_program_lines = 0
    read_files = []

    for test_case in range(sir_programs_tests_number[program_name]):
        # to open the correct file, we must remember that the folders and the json files are
        # numbered from 1 and not from 0
        if program_name == "gzip":
            json_name = "allfile"
        else:
            json_name = program_name
        test_case_json = open("datasets/sir_programs/" + program_name + "/json_" + program_name + "/t" + str(
            test_case + 1) + "/" + json_name + str(test_case + 1) + ".gcov.json")

        # read the JSON object as a dictionary
        json_data = json.load(test_case_json)

        # to have coverage % we will need the total number of lines to cover
        for file in json_data["files"]:
            if file["file"] not in read_files:
                total_program_lines += len(file["lines"])
                read_files.append(file["file"])

        # for programs tested above more than one file, the initial row of a file will start from
        # the final row of the preceding file
        i = 0
        for file in json_data["files"]:
            line_count_start = i
            for line in file["lines"]:
                # if a line is executed, we want to remember FOR THAT LINE which are the tests
                # that executed it, and we want to increment the execution cost
                if line["unexecuted_block"] == False:
                    # the test suite exec cost = sum of the exec freq. of each executed basic block
                    # by each test case
                    # test_suite_execution_cost += line["count"]
                    test_case_execution_cost += line["count"]

                    if (line_count_start + line["line_number"]) not in executed_lines_test_by_test:
                        executed_lines_test_by_test[line_count_start + line["line_number"]] = [test_case]
                    else:
                        executed_lines_test_by_test[line_count_start + line["line_number"]].append(test_case)
                    executed_lines_test_by_test[line_count_start + line["line_number"]].sort()

                    if test_case not in test_coverage_line_by_line:
                        test_coverage_line_by_line[test_case] = [line_count_start + line["line_number"]]
                    else:
                        test_coverage_line_by_line[test_case].append(line_count_start + line["line_number"])
                    test_coverage_line_by_line[test_case].sort()
                i = line["line_number"]

        # saving the total amount of execution cost for this test case and resetting for the next
        execution_cost_test_by_test[test_case] = test_case_execution_cost
        test_case_execution_cost = 0

        test_case_json.close()

    return execution_cost_test_by_test, executed_lines_test_by_test, test_coverage_line_by_line, total_program_lines


# we can now gain all the historical information about past fault detection
faults_dictionary = {"flex": None, "grep": None, "gzip": None, "sed": None}

for sir_program in sir_programs:
    faults_dictionary[sir_program] = (get_fault_list(sir_program))

#we can now gain costs and coverage information

test_cases_costs = {"flex":None,"grep":None,"gzip":None,"sed":None}
executed_lines_test_by_test = {"flex":None,"grep":None,"gzip":None,"sed":None}
test_coverage_line_by_line = {"flex":None,"grep":None,"gzip":None,"sed":None}
total_program_lines = {"flex":None,"grep":None,"gzip":None,"sed":None}

for sir_program in sir_programs:
    costs_and_coverage = cost_and_coverage_information_gathering(sir_program)
    test_cases_costs[sir_program] = costs_and_coverage[0]
    executed_lines_test_by_test[sir_program] = costs_and_coverage[1]
    test_coverage_line_by_line[sir_program] = costs_and_coverage[2]
    total_program_lines[sir_program] = costs_and_coverage[3]

for sir_program in sir_programs:
    for i in range(sir_programs_tests_number[sir_program]):
        if i not in test_coverage_line_by_line[sir_program].keys():
            test_coverage_line_by_line[sir_program][i] = []

# Store the dictionaries in a list with their names
dictionaries = {
    "faults_dictionary": faults_dictionary,
    "test_cases_costs": test_cases_costs,
    "executed_lines_test_by_test": executed_lines_test_by_test,
    "test_coverage_line_by_line": test_coverage_line_by_line,
    "total_program_lines": total_program_lines
}

# Convert each dictionary into a JSON file
for name, data in dictionaries.items():
    with open(f"datasets/sir_programs/{name}.json", "w") as file:
        json.dump(data, file)

print("Dictionaries have been saved as JSON files!")