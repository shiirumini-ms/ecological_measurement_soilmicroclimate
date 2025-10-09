# import libraries
import geopandas as gpd
import matplotlib.pyplot as plt
import shapely
from shapely.geometry import Point
from shapely.geometry import Polygon
import numpy as np
import pandas as pd
import contextily as ct

# read shapefile of Ben Lawers boundary
benlawers = gpd.read_file("nnr.shp")
