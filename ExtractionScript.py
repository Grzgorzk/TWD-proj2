#!/usr/bin/env python
# coding: utf-8


import sys
import copy
import json
import pandas as pd


def locationToLongLat(location):
    return location['longitudeE7'], location['latitudeE7']


def durationToTimeStampsMS(duration):
    return duration['startTimestampMs'], duration['endTimestampMs']


def niceASDict(ActivitySegmentObj):
    t = {}
    t['StartingLongitude'], t['StartingLatitude'] = locationToLongLat(ActivitySegmentObj['startLocation'])
    t['EndingLongitude'], t['EndingLatitude'] = locationToLongLat(ActivitySegmentObj['endLocation'])
    t['StartingtimeStampInMS'], t['EndtimeStampInMS'] = durationToTimeStampsMS(ActivitySegmentObj['duration'])
    t['ActivityType'] = ActivitySegmentObj['activityType']
    t['Distance'] = ActivitySegmentObj['distance']

    return t


def addNiceDictToBigNiceDict(bigDict, smallDict):
    ND = copy.deepcopy(bigDict)
    for key in bigDict.keys():
        if key in ND.keys():
            ND[key].append(smallDict[key])
        else:
            raise Exception('keys in bigDict and smallDict dont match')
    return ND


def points(Object):
    returnDict = {key: [] for key in POINTS_KEYS}
    try:
        for point in Object['simplifiedRawPath']['points']:
            returnDict['Longitude'].append(point['lngE7'])
            returnDict['Latitude'].append(point['latE7'])
            returnDict['TimeStampInMS'].append(point['timestampMs'])
    except KeyError:  # niektore aktywnosci nie maja w ogole punktow
        return {key: [] for key in POINTS_KEYS}
    return returnDict


def concatBigDicts(dict1, dict2):
    newDict = {key: [] for key in dict1.keys()}
    for key in newDict.keys():
        try:
            newDict[key] = dict1[key] + dict2[key]
        except KeyError:
            raise exception('Keys in dicts dont match')
    return newDict


def nicePVDict(PlaceVisitObj):
    d = {}
    d['Longitude'], d['Latitude'] = locationToLongLat(PlaceVisitObj['location'])
    d['PlaceId'] = PlaceVisitObj['location']['placeId']
    d['Name'] = PlaceVisitObj['location']['name']
    d['StartTimeStampInMS'], d['EndTimeStampInMS'] = durationToTimeStampsMS(PlaceVisitObj['duration'])
    return d


AS_KEYS = ['StartingLongitude',
           'StartingLatitude',
           'EndingLongitude',
           'EndingLatitude',
           'StartingtimeStampInMS',
           'EndtimeStampInMS',
           'ActivityType',
           'Distance']  # kolumny w as.csv

POINTS_KEYS = ['Longitude',
               'Latitude',
               'TimeStampInMS']

PV_KEYS = ['PlaceId',
           'Longitude',
           'Latitude',
           'StartTimeStampInMS',
           'EndTimeStampInMS',
           'Name']

if __name__ == '__main__':
    if len(sys.argv) < 4:
        raise Exception('too few args')

    targetloc = sys.argv[1]
    CharacteristicFileName = sys.argv[2]
    JSONlocs = sys.argv[3:]
    AS, PV = [], []
    for JSONloc in JSONlocs:
        print(JSONlocs, "\n")
        with open(JSONloc, encoding='utf-8') as data:
            load = json.load(data)
        for timelineObject in load['timelineObjects']:
            if 'activitySegment' in timelineObject.keys():
                AS.append(timelineObject['activitySegment'])
            else:
                PV.append(timelineObject['placeVisit'])

    PointsDict = {key: [] for key in POINTS_KEYS}
    ASDict = {key: [] for key in AS_KEYS}
    for ASObject in AS:
        ASDict = addNiceDictToBigNiceDict(ASDict, niceASDict(ASObject))
        PointsDict = concatBigDicts(PointsDict, points(ASObject))
    ASdf = pd.DataFrame(ASDict)
    ASdf.to_csv(targetloc + '/ActivitySegment' + CharacteristicFileName + '.csv')

    PVDict = {key: [] for key in PV_KEYS}
    for PVObject in PV:
        PVDict = addNiceDictToBigNiceDict(PVDict, nicePVDict(PVObject))
        PointsDict = concatBigDicts(PointsDict, points(PVObject))
    PVdf = pd.DataFrame(PVDict)
    PVdf.to_csv(targetloc + '/PlacesVisited' + CharacteristicFileName + '.csv')

    Pointsdf = pd.DataFrame(PointsDict)
    Pointsdf.to_csv(targetloc + '/Points' + CharacteristicFileName + '.csv')
############################################################################


