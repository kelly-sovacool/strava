""" Download, analyze, and plot activity data from Strava

Usage:
    fitness-geek.py <path_to_api_token> [--output-file=<output-file>]

options:
    -h --help   Display this help message.
    -o --output-file    Output filename to save activity data as a CSV file. [default: data/activities.csv]
"""
import docopt
import pandas as pd
import os
import stravalib

def main(args):
    # TODO: use OAuth for access token
    with open(args['<path_to_api_token>'], 'r') as file:
        token = file.readlines().strip()
    client = stravalib.client.Client(access_token = token)
    activities = client.get_activities()
    interesting_columns = ['average_speed', 'distance', 'elapsed_time', 'total_elevation_gain', 'type', 'start_date_local']
    activity_data = pd.DataFrame([[activity.to_dict().get(column) for column in interesting_columns] for activity in activities], columns=interesting_columns)
    
    if not os.path.exists('data'):
        os.mkdir('data')
    activity_data.to_csv(args['--output-file'])

    # TODO: generate pretty html of most recent activities
    # TODO: plot activities/distance over time (week/month/year)

if __name__ == "__main__":
    main(docopt.docopt(__doc__))
