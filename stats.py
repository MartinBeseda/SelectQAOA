import ast
import csv

import pandas as pd
from matplotlib import pyplot as plt
from scipy.stats import mannwhitneyu

def a12(lst1,lst2,rev=True):
  "how often is x in lst1 more than y in lst2?"
  more = same = 0.0
  for x in lst1:
    for y in lst2:
      if x==y : same += 1
      elif rev and x > y : more += 1
      elif not rev and x < y : more += 1
  return (more + 0.5*same)  / (len(lst1)*len(lst2))

def stat_test(app1, app2):
    statistic, pvalue = mannwhitneyu(app1, app2, alternative='two-sided')

    # Calculate the A12 effect size using Vargha and Delaney's formula
    a12_effect_size = a12(app1, app2)

    return pvalue, a12_effect_size

def plotter(data_name, bootqa_costs, qaoa_costs, qtcs_costs, bootqa_rates, qaoa_rates, qtcs_rates):
    plt.figure(figsize=(8, 6))

    # Plot bootqa rates vs costs
    plt.scatter(bootqa_costs, bootqa_rates, color='blue', label='bootqa')

    # Plot qtcs rates vs costs
    plt.scatter(qaoa_costs, qaoa_rates, color='red', label='qaoa')

    # Plot qtcs rates vs costs
    plt.scatter(qtcs_costs, qtcs_rates, color='blue', label='qtcs')

    plt.xlabel('Costs')
    plt.ylabel('Rates')
    plt.title('Rates vs Costs for ' + data_name)
    plt.legend()
    plt.grid(True)

    # Add lines from red points to x-axis
    for i in range(len(qaoa_costs)):
        plt.plot([qaoa_costs[i], qaoa_costs[i]], [qaoa_costs[i], 0], color='red', linestyle='--')

    for j in range(len(qtcs_costs)):
        plt.plot([qtcs_costs[i], qtcs_costs[i]], [qtcs_costs[i], 0], color='blue', linestyle='--')

    # Add line from red points to infinity
    max_qaoa_cost = max(qaoa_costs)
    max_qtcs_costs = max(qtcs_costs)
    plt.hlines(bootqa_rates, max_qaoa_cost, max_qaoa_cost + 1, colors='red', linestyles='--')
    plt.hlines(bootqa_rates, max_qtcs_costs, max_qtcs_costs + 1, colors='blue', linestyles='--')

    plt.show()

if __name__ == '__main__':
    data_names = ["gsdtsr","paintcontrol"]
    data_names_stats = {"gsdtsr": [], "paintcontrol": []}

    for data_name in data_names:
        file_path_bootqa = f"./results/bootqa/{data_name}.csv"

        # read sum.csv
        sum_df_bootqa = pd.read_csv(file_path_bootqa)

        # Get the lists time_list and rate_list and the variable coverage_level
        final_test_suite_costs_bootqa = ast.literal_eval(sum_df_bootqa['final_test_suite_costs'].iloc[-1])
        final_failure_rates_bootqa = ast.literal_eval(sum_df_bootqa['final_failure_rates'].iloc[-1])

        file_path_qtcs = (f".results/selectqa/{data_name}.csv")

        # read qaoa results
        sum_df_qtcs = pd.read_csv(file_path_qtcs)

        # Get the lists time_list and rate_list and the variable coverage_level
        final_test_suite_costs_qtcs = ast.literal_eval(sum_df_qtcs['final_test_suite_costs'].iloc[-1])
        final_failure_rates_qtcs = ast.literal_eval(sum_df_qtcs['final_failure_rates'].iloc[-1])

        file_path_qaoa = (f".results/selectqaoa/{data_name}.csv")

        # read qaoa results
        sum_df_qaoa = pd.read_csv(file_path_qaoa)

        # Get the lists time_list and rate_list and the variable coverage_level
        final_test_suite_costs_qaoa = ast.literal_eval(sum_df_qaoa['final_test_suite_costs'].iloc[-1])
        final_failure_rates_qaoa = ast.literal_eval(sum_df_qaoa['final_failure_rates'].iloc[-1])

        cost_p_value_qaoa_bootqa, cost_a12_qaoa_bootqa = stat_test(final_test_suite_costs_qaoa,final_test_suite_costs_bootqa)
        rate_p_value_qaoa_bootqa, rate_a12_qaoa_bootqa = stat_test(final_failure_rates_qaoa,final_failure_rates_bootqa)
        cost_p_value_qaoa_qtcs, cost_a12_qaoa_qtcs = stat_test(final_test_suite_costs_qaoa, final_test_suite_costs_qtcs)
        rate_p_value_qaoa_qtcs, rate_a12_qaoa_qtcs = stat_test(final_failure_rates_qaoa, final_failure_rates_qtcs)
        cost_p_value_qtcs_bootqa, cost_a12_qtcs_bootqa = stat_test(final_test_suite_costs_qtcs, final_test_suite_costs_bootqa)
        rate_p_value_qtcs_bootqa, rate_a12_qtcs_bootqa = stat_test(final_failure_rates_qtcs, final_failure_rates_bootqa)

        data_names_stats[data_name] = [(cost_p_value_qaoa_bootqa,cost_a12_qaoa_bootqa),
                                       (rate_p_value_qaoa_bootqa, rate_a12_qaoa_bootqa),
                                       (cost_p_value_qaoa_qtcs, cost_a12_qaoa_qtcs),
                                       (rate_p_value_qaoa_qtcs, rate_a12_qaoa_qtcs),
                                       (cost_p_value_qtcs_bootqa, cost_a12_qtcs_bootqa),
                                       (rate_p_value_qtcs_bootqa,rate_a12_qtcs_bootqa)]

        # let's plot the graphic
        plotter(data_name,final_test_suite_costs_bootqa,final_test_suite_costs_qaoa,final_test_suite_costs_qtcs,
                final_failure_rates_bootqa,final_failure_rates_qaoa,final_failure_rates_qtcs)

    with open('.results/stats_results.csv', 'w', newline='') as csvfile:
        field_names = ["data_name", "cost_p_value_qaoa_bootqa", "cost_a12_qaoa_bootqa",
                       "rate_p_value_qaoa_bootqa", "rate_a12_qaoa_bootqa", "cost_p_value_qaoa_qtcs",
                       "cost_a12_qaoa_qtcs", "rate_p_value_qaoa_qtcs", "rate_a12_qaoa_qtcs",
                       "cost_p_value_qtcs_bootqa", "cost_a12_qtcs_bootqa",
                       "rate_p_value_qtcs_bootqa", "rate_a12_qtcs_bootqa"]
        writer = csv.DictWriter(csvfile,fieldnames=field_names)

        # writing the header
        writer.writeheader()

        # rows
        for data_name,values in data_names_stats.items():
            row = {
                'data_name': data_name,
                'cost_p_value_qaoa_bootqa': values[0][0],
                'cost_a12_qaoa_bootqa': values[0][1],
                'rate_p_value_qaoa_bootqa': values[1][0],
                'rate_a12_qaoa_bootqa': values[1][1],
                'cost_p_value_qaoa_qtcs': values[2][0],
                'cost_a12_qaoa_qtcs': values[2][1],
                'rate_p_value_qaoa_qtcs': values[3][0],
                'rate_a12_qaoa_qtcs': values[3][1],
                'cost_p_value_qtcs_bootqa': values[4][0],
                'cost_a12_qtcs_bootqa': values[4][1],
                'rate_p_value_qtcs_bootqa': values[5][0],
                'rate_a12_qtcs_bootqa': values[5][1]
            }
            writer.writerow(row)
