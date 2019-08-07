""" Download, analyze, and plot activity data from Strava

Usage:
    fitness-geek.py [--token=<file> --output=<file>]

options:
    -h --help               Display this help message.
    -t --token=<file>       Path to file containing the API token. [default: .token]
    -o --output=<file>      Output filename to save activity data as a CSV file. [default: data/activities.csv]
"""
import docopt
import pandas as pd
import os
import pprint
import stravalib


def main(args):
    # TODO: use OAuth for access token
    with open(args['--token'], 'r') as token_file:
        token = token_file.readline().strip()
        print(token)
    client = stravalib.client.Client(access_token=token)
    activities = client.get_activities()
    pprint.pprint([activity.to_dict() for activity in activities])
    interesting_columns = ['average_speed', 'distance', 'elapsed_time', 'total_elevation_gain', 'type',
                           'start_date_local']
    activity_data = pd.DataFrame(
        [[activity.to_dict().get(column) for column in interesting_columns] for activity in activities],
        columns=interesting_columns)

    if not os.path.exists('data'):
        os.mkdir('data')
    activity_data.to_csv(args['--output-file'])

    # TODO: generate pretty html of most recent activities
    # TODO: plot activities/distance over time (week/month/year)


if __name__ == "__main__":
    main(docopt.docopt(__doc__))
