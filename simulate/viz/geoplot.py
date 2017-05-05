#! /usr/bin/env python

import networkx as nx
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap as Basemap
from mpl_toolkits.axes_grid1 import make_axes_locatable
import os
import glob
import optparse
import sys
import math
import pprint

from collections import defaultdict
import numpy as np
import matplotlib.cm as cm
import matplotlib.colors as colors
from matplotlib.font_manager import FontProperties
from math import sin, cos, sqrt, atan2, radians

from showEdgeCongestion import *
import CommonConf
import CommonViz

__all__ = ['plot_graph']

# Hide axes as per http://matplotlib.sourceforge.net/users/customizing.html
plt.rc('axes',linewidth=0)

opt = optparse.OptionParser()
opt.add_option('--file', '-f', help="process file")
opt.add_option('--output_dir', '-o', help="process directory")
opt.add_option('--bluemarble', action="store_true",
               default=False, help="Use Blue Marble background image")

opt.add_option('--back_image', help="Image to use as background")
opt.add_option('--edge_label', help="attribute to use for edge_label")

opt.add_option('--link_util', action="store_true", default=False, help="plot link utilizations")
opt.add_option('--paths', action="store_true", default=False, help="plot paths")

opt.add_option('--showalg', action="store_true", default=False, help="show algorithm name")

opt.add_option('--expand_scale', type="float", default=1,
                help="Scale to expand map beyond outermost nodes"
               " Example range: 1-5")

opt.add_option('--crop_right', type="float", default=0,
                help="Fraction of figure to crop from right"
               " Example range: 0-1")

opt.add_option('--crop_left', type="float", default=0,
                help="Fraction of figure to crop from left"
               " Example range: 0-1")

opt.add_option('--crop_top', type="float", default=0,
                help="Fraction of figure to crop from top"
               " Example range: 0-1")

opt.add_option('--crop_bottom', type="float", default=0,
                help="Fraction of figure to crop from top"
               " Example range: 0-1")


opt.add_option('--external_node_scale', type="float", default=0,
                help="Scale for external nodes to be spaced out. Default 0"
               "(No external nodes plotted). Example range: 1-10")
opt.add_option('--labels', action="store_true",
               default=False, help="Print node labels")
opt.add_option('--numeric_labels', action="store_true",
               default=False, help="Use numeric node labels")

opt.add_option('--debuglabels', action="store_true", default=False, help="Print debugging (numeric) node labels")
opt.add_option('--no_update', action="store_true", default=False, help="Only render image if destination file not present (don't update existing images). Faster for batch-processing large datasets.")

opt.add_option('--res', help="Resolution level from 0 (none) to 5"
               " (full)", type="int", default=1)

opt.add_option('--node_size', help="Size to plot nodes as", type="float", default=10)
opt.add_option('--image_scale',
               help="Image quality level for bluemarble and warp images"
               "default 0 (none), 1 is high",
               type="float", default=0)

opt.add_option('--max_scale', help="Expected Max Congestion", type="float", default=10)
opt.add_option('--label_font_size', help="Size to plot nodes labels as", type="float", default=10)
opt.add_option('--country_color', help="Background color for countries", type="str", default="#dddddd")
opt.add_option('--default_edge_color', help="Color for edges", type="str", default="")
opt.add_option('--edge_label_order', help="Ordering for edge colors", type="str", default="")
opt.add_option('--edge_colormap', help="Colormap for edge colors. Note most colormaps have a reverse _r eg cool_r", type="str", default="")

opt.add_option('--line_width', help="Size to plot lines as", type="float", default=1)
opt.add_option('--heatmap', action="store_true",
               default=False, help="Plot heatmap")
opt.add_option('--title', action="store_true",
               default=False, help="Add title to graph")
opt.add_option('--highlight_outliers', action="store_true",
               default=False, help="Highlight outlier nodes")
opt.add_option('--straight_line', action="store_true", default=False,
               help="Draw straight line between nodes instead of great circle")


options, args = opt.parse_args()

#TODO: make mark outliers handle great circle - or disable option
# and warn user

#TODO: allow user to plot using normal scatter plot, ie not only basemap
#TODO: add option to mark outliers

#TODO: split into functions

path_colors = ['#aa00aa', '#006600', '#aa0000', '#0000ff']

def get_offset(color):
    return path_colors.index(color) - len(path_colors)/2

def get_max_exp_utilization(network_name):
  (xs, ysPerSolver, ydevsPerSolver) = CommonViz.parseData(
      '../../expData/'+network_name, 'MaxExpCongestionVsIterations', set())
  return max([max(ys) for ys in ysPerSolver.values()])


def getExpUtils(network_name):
    all_congestions = parse_congestions_file('../../expData/'+network_name+'/EdgeExpCongestionVsIterations.dat')
    return all_congestions

def get_links_in_path(paths_file):
    read_paths = False
    index = len(path_colors)/2
    link_colors = dict()
    with open(paths_file) as f:
        for l in f.readlines():
            if "h47 -> h1 :" in l:
                read_paths = True
                continue
            if read_paths:
                if len(l.strip()) == 0:
                    break
                print l.split('@')
                path = l.split('@')[0].strip()[1:-1].split(', ')
                weight = l.split('@')[1].strip()

                for link in path:
                    link_colors[link] = link_colors.get(link, []) + [path_colors[index]]
                index = (index + 1)%len(path_colors)
    return link_colors

def plot_graph(G, solver, output_path, title=False, use_bluemarble=False,
               back_image = False,
               use_labels=False,
               expand_scale = 1,
               numeric_labels=False, external_node_scale=0,
               basemap_resolution_level = 1,
               node_size = 10, line_width = 1,
               manual_image_scale = 0,
               country_color = "#666666",
               max_scale=10,
               label_font_size=10,
               no_update = False,
               show_figure=False,
               user_default_edge_color = None,
               edge_label_order = [],
               edge_font_size =3,
               edge_colormap = None,
               standalone = False, # if called programatically
               edge_label_attribute=False):

    exputils = getExpUtils(G.name)
    output_path = os.path.abspath(output_path)
    basemap_resolution_levels = {
        0: None,
        1: 'c',
        2: 'l',
        3: 'i',
        4: 'h',
        5: 'f'
    }
    if (basemap_resolution_level and basemap_resolution_level in
        basemap_resolution_levels):
        basemap_resolution = basemap_resolution_levels[basemap_resolution_level]
    else:
        basemap_resolution = basemap_resolution_levels[1]

    #TODO: make title allow to send through a string, or alternatively specify
    #to come from network name
    #TODO: clean up this handling when called programatically
    network_name = G.name
    if network_name == "":
        network_name = "network"  #default name

    #TODO: look at case where direct link A->B with geocoded co-ords
    # gets overwritten with inferred nodes if multiple paths between A-B
    try:
        edge_label_order.upper() # test if string-like
        edge_label_order = [x.strip() for x in edge_label_order.split(",")]
    except AttributeError:
        pass # not string-like

    out_file = "{0}/{1}".format(output_path, network_name.replace(" ", "_"))
    plt_filename_pdf = "%s_%s.pdf" % (out_file, solver)

    if no_update:
# see if destination already exists
        if (plt_filename_pdf and os.path.exists(plt_filename_pdf)):
            print "Output file already exists for %s, skipping" % network_name
            return

#TODO: make easy way to turn on/off custom geant plotting: use config settings?

    # Remove any external nodes
    external_nodes = []
    if external_node_scale:
        external_nodes = [n for n, data in G.nodes(data=True)
                          if 'Internal' in data and data['Internal'] == 0]


    geocoded_cities = [ n for n, data in G.nodes(data = True)
                if 'Latitude' in data and 'Longitude' in data]

    #TODO make this command line argument and parameter
    if external_node_scale:
        # Find internal nodes connected to an external node
        boundary_nodes = nx.node_boundary(G, external_nodes)
        for bound_node in boundary_nodes:
            if ('Latitude' in G.node[bound_node] and 'Longitude' in
                G.node[bound_node]):
                bound_lat = G.node[bound_node]['Latitude']
                bound_lon = G.node[bound_node]['Longitude']
                # Find external neighbours
                ext_neighbors = [n for n in G.neighbors(bound_node) if n in
                                 external_nodes]
                for index, ext_neigh in enumerate(ext_neighbors):
                    theta = 2*math.pi*(float(index)/len(ext_neighbors))
                    radius = external_node_scale*1.0/10
                    x = radius * math.cos(theta)
                    y = radius * math.sin(theta)
                    G.node[ext_neigh]['Latitude'] = bound_lat + x
                    G.node[ext_neigh]['Longitude'] = bound_lon + y
            else:
                print "no lat/lon for %s (%s)" % (G.node[bound_node]['label'],
                                                  G.node[bound_node]['Network'])
                # Don't plot ext connections from this node
                ext_neighbors = [n for n in G.neighbors(bound_node) if n in
                                 external_nodes]
                for ext_neigh in ext_neighbors:
                    external_nodes.remove(ext_neigh)

    #TODO: make sure external nodes don't interfere with subsequent calculations
    # on geocoded nodes

    # Sanity check don't try to infer location if no geocoded places
    if len(geocoded_cities) == 0:
        print "No geocoded nodes in {0}, skipping".format(network_name)
        return

    hyperedge_nodes = [ n for n, data in G.nodes(data = True)
                    if (('hyperedge' in data and data['hyperedge'] == 1)
                    and ('Internal' in data and data['Internal'] == 1))]

    # Handle non geocoded places, as give misleading appearance of disconnected
    # graph
    non_geocoded_cities = [n for n in G.nodes()
                           if (n not in geocoded_cities
                               and n not in hyperedge_nodes
                              and n not in external_nodes)]
    print "Non-geocoded:", non_geocoded_cities
    for n in non_geocoded_cities:
        if G.degree(n) < 1:
            # Single node eg ---0
            G.remove_node(n)
        elif G.degree(n) > 1:
            # Infer coordinates as mean of neighbors
            he_lats=[]
            he_lons=[]
            for neigh in G.neighbors(n):
                if 'Latitude' in G.node[neigh]:
                    he_lats.append(G.node[neigh]['Latitude'])
                if 'Longitude' in G.node[neigh]:
                    he_lons.append(G.node[neigh]['Longitude'])
            if len(he_lats) > 0 and len(he_lons) > 0:
                G.node[n]['Latitude'] = np.mean(he_lats)
                G.node[n]['Longitude'] = np.mean(he_lons)
            print G.node[n]


    # work out how to do multiple hyperedges connected to each other eg deltacom
    #toDO: make this option argument command line-able
    # TODO: make this non-geocoded nodes rather than hyperedge nodes
    for node in hyperedge_nodes:
        he_lats = []
        he_lons = []
        for neigh in G.neighbors(node):
            if 'Latitude' in G.node[neigh]:
                he_lats.append( G.node[neigh]['Latitude'] )
            if 'Longitude' in G.node[neigh]:
                he_lons.append( G.node[neigh]['Longitude'] )
        if len(he_lats) > 0 and len(he_lons) > 0:
            G.node[node]['Latitude'] = np.mean(he_lats)
            G.node[node]['Longitude'] = np.mean(he_lons)

    #TODO: note that if node removed, it loses its label which won't appear if
    # using --labels command line argument
    # add back in the nodes
    inferred_nodes = []
    for s, t, edge_data in G.edges(data=True):
        if 'removed_nodes' in edge_data:
            removed_nodes = edge_data['removed_nodes']
            removed_count = len(removed_nodes)
            # now interpolate
            # eg if 2 nodes, want to space 1/(2+1) = 1/3 of way along
            del_x = (G.node[s]['Latitude'] -
                    G.node[t]['Latitude'])/(removed_count+1)
            del_y = (G.node[s]['Longitude'] -
                    G.node[t]['Longitude'])/(removed_count+1)

            # Add node(s) back in
            index = 1
            for node, data in removed_nodes.items():
                data['Latitude'] = G.node[s]['Latitude'] - (index * del_x)
                data['Longitude'] = G.node[s]['Longitude'] - (index * del_y)
                G.add_node(node, data)
                index += 1
                # Plot as an inferred node
                inferred_nodes.append(node)

            # Remove edge, will be replaced by edge between nodes
            G.remove_edge(s,t)
            # List of nodes to reconnect, form s--rem[0]--rem[1]--....--t
            reconnect_list = [s] + removed_nodes.keys() + [t]
            # Join as pairs
            # based on http://stackoverflow.com/q/2829887
            for (a, b) in zip(reconnect_list, reconnect_list[1:]):
                G.add_edge(a, b, edge_data)


    # Get only hyperedge nodes in graph
    # TODO: see if can use nbunch in networkx
    hyperedge_nodes = [n for n in hyperedge_nodes if n in G]

    lats = []
    lons = []
    latlon_node_index = []
    for n, data in G.nodes(data=True):
        lats.append(data['Latitude'])
        lons.append(data['Longitude'])
        # Also store the node order for retrieval
        latlon_node_index.append(n)

    # check that not all co-located points
    # Check if smaller than typical rounding error
    # Otherwise get error when doing Basemap calculations
    rnd_err = 0.0001
    if ( np.std(lats) < rnd_err and np.std(lons) < rnd_err):
        # No std dev -> all points == mean, ie all same
        print ("All nodes in {0} have same location, "
            "skipping").format(network_name)
        return

    # corner lats and lons
    llcrnrlon = min(lons)
    llcrnrlat = min(lats)
    urcrnrlon = max(lons)
    urcrnrlat = max(lats)

    # expand so have frame around border nodes
    # expand by % of distance between border nodes
    width_expand_factor = height_expand_factor = 1.0 * expand_scale/10

    # Zoom out if small map (threshold found empirically)
    map_width = abs(urcrnrlon - llcrnrlon)
    map_height = abs(urcrnrlat - llcrnrlat)

    # make minimum width so don't get long skinny map
    if 4*map_width < map_height:
        width_expand_factor = 4*width_expand_factor
    elif 2*map_width < map_height:
        width_expand_factor = 2*width_expand_factor
    elif 4*map_height < map_width:
        height_expand_factor = 4*height_expand_factor
    # removed this as can cause some US maps to be not plotted
    #elif 2*map_height < map_width:
    #    print 4
    #    height_expand_factor = 2*height_expand_factor

    image_scale = 0.5
    if (map_width * map_height ) < 1:
        width_expand_factor =  width_expand_factor*10
        height_expand_factor = height_expand_factor*10
        image_scale = 0.9
    elif (map_width * map_height ) < 5:
        width_expand_factor =  width_expand_factor*4
        height_expand_factor = height_expand_factor*4
        image_scale = 0.8
    elif (map_width * map_height ) < 10:
        width_expand_factor =  width_expand_factor*3
        height_expand_factor = height_expand_factor*3
        image_scale = 0.7
    elif (map_width * map_height ) < 20:
        width_expand_factor =  width_expand_factor*2.5
        height_expand_factor = height_expand_factor*2.5
        image_scale = 0.65
    elif (map_width * map_height ) < 50:
        width_expand_factor =  width_expand_factor*2
        height_expand_factor = height_expand_factor*2
        image_scale = 0.65
    elif (map_width * map_height ) < 100:
        width_expand_factor =  width_expand_factor*1.5
        height_expand_factor = height_expand_factor*1.5
        image_scale = 0.6
    elif (map_width * map_height ) < 200:
        width_expand_factor =  width_expand_factor*1
        height_expand_factor = height_expand_factor*1
        image_scale = 0.5

    # Quality scaling factor for large blue marble maps
    #TODO: merge with above scaling
    if use_bluemarble or back_image:
        if (map_width * map_height) > 4000:
            image_scale = 0.25
        elif (map_width * map_height) > 1000:
            image_scale = 0.4

    # manual boundaries for geant
    # left, bottom, right, top
    if G.graph.get('Network') == 'European NRENs':
        (llcrnrlon, llcrnrlat, urcrnrlon, urcrnrlat) = (-6, 35, 35, 58)

    # over write if specified manually
    if manual_image_scale:
        image_scale = manual_image_scale

    margin_lon = width_expand_factor * abs(urcrnrlon - llcrnrlon)
    margin_lat = height_expand_factor * abs(urcrnrlat - llcrnrlat)

    # and expand
    llcrnrlon -= margin_lon
    llcrnrlat -= margin_lat
    urcrnrlon += margin_lon
    urcrnrlat += margin_lat

    # Crop image
    llcrnrlon += options.crop_left * abs(urcrnrlon - llcrnrlon)
    llcrnrlat += options.crop_bottom * abs(urcrnrlat - llcrnrlat)
    urcrnrlon -= options.crop_right * abs(urcrnrlon - llcrnrlon)
    urcrnrlat -= options.crop_top * abs(urcrnrlat - llcrnrlat)

    # Stop wrapping around at date-line, due to expansions above
    llcrnrlon = max(llcrnrlon, -179)
    urcrnrlon = min(urcrnrlon, 179)

    # ensure that lat fits within (-90,90) once expanded
    # Use 85 as boundary, as mercator becomes very distorted when near poles
    # We don't have many (ant)artic maps
    llcrnrlat = max(llcrnrlat, -60)
    urcrnrlat = min(urcrnrlat, 65)

    lat_1 = (urcrnrlat + llcrnrlat)/2
    lon_0 = (urcrnrlon + llcrnrlon)/2
    #TODO: see if can clear old figures rather than new one each time
    #TODO: see if stil need this try catch block now do stdev check

    try:
        # Draw the map
        plt.clf()
        fig = plt.figure()
        # Create axes to allow adding of text relative to map
        ax = fig.add_subplot(111)
        m = Basemap(resolution = basemap_resolution,
                    projection='merc', llcrnrlat = llcrnrlat,
                    urcrnrlat = urcrnrlat,  llcrnrlon = llcrnrlon,
                    urcrnrlon = urcrnrlon, lat_ts = lat_1)

    except ZeroDivisionError, e:
        print "Error {0}".format(e)
        # Do the next map
        return

    # Convert lats and lons into x,y for plotting
    mx, my = m(lons, lats)

    pos = {}
    for index in range(len(mx)):
        # Extract the converted lat lons for each node
        node = latlon_node_index[index]
        pos[node] = (mx[index], my[index])

    # and labels
    if use_labels or numeric_labels:
        labels = {}
        for n, data in G.nodes(data = True):
            #TODO: make numeric labels an option
            """
            if 'Network' in data and data['Network'] == 'GEANT':
                use_labels = True
                labels[n] = data['label']

            continue
        """
            if numeric_labels:
                labels[n] = n
            else:
                labels[n] =  data['label']

    # Labels for certain nodes for ECMP link util plot
    if solver == 'ecmp' and options.link_util:
        use_labels = True
        labels = {}
        for n, data in G.nodes(data = True):
            if data['label'] in ['Boston', 'Halifax']:
                labels[n] =  data['label']
    # Labels for certain nodes for path plot
    if options.paths:
        use_labels = True
        labels = {}
        for n, data in G.nodes(data = True):
            if data['label'] in ['Seattle', 'Miami']:
                labels[n] =  data['label']


    if numeric_labels:
        print "Labels:"
        for n, data in G.nodes(data = True):
            print "%s: %s" % (n, data['label'])

    if not country_color.startswith("#"):
        country_color = "#" + country_color

    if use_bluemarble:
        m.bluemarble(scale = image_scale)
    elif back_image:
        #TODO: while loop to catch memory error and try with lower scale
        #image_scale = 0.8
        print "warp scale %s " % image_scale
        m.warpimage(image = back_image,
                   scale = image_scale)
    else:
        if G.graph.get('Network') == 'European NRENs':
            m.fillcontinents(color='#9ACEEB')
        else:
            m.fillcontinents(color=country_color)

        #m.fillcontinents()

    # Colours depending on if using bluemarble image or white background
    if use_bluemarble or back_image:
        node_color ="#FF8C00"
        font_color = "w"
        default_edge_color = "#bbbbbb"
        title_color = "w"
        caption_color = 'w'
        colormap = cm.autumn
        #colormap = cm.jet
        #country_color = '#666666'
    else:
        if use_labels:
            node_color = 'gray'
        else:
            node_color = "k"
        font_color = "k"
        default_edge_color = "#666666"
        #default_edge_color = "0.8"
        title_color = "k"
        caption_color = 'gray'
        colormap = cm.jet
        #colormap = cm.autumn
        #colormap = cm.winter
        #colormap = cm.summer
        #country_color = '#AAAAAA'
        #country_color = '#DDDDDD'
        if G.graph.get('Network') == 'European NRENs':
            default_edge_color = '0.1'

    if edge_colormap:
        colormap = plt.get_cmap(edge_colormap)

    if options.default_edge_color:
        user_default_edge_color = options.default_edge_color

    if user_default_edge_color:
        default_edge_color = user_default_edge_color

    m.drawcountries(linewidth = 0.05, zorder=1.5, color = country_color)
    m.drawcoastlines(linewidth = 0.1, zorder=1, color = country_color)

    delta_zorders = { 'added': 0.2, 'removed': 0.3, 'modified': 0.1, '': 0} # used default color if no delta

    # Try Great Circle plot
    delta_colors = { 'added': '#339966', 'removed': 'r', 'modified': '#3366ff',
            '': default_edge_color} # used default color if no delta
    #delta_styles = { 'added': 'dashed', 'removed': 'dotted', 'modified': 'dashdot', '': 'solid'}
    delta_styles = { 'added': 'solid', 'removed': 'solid', 'modified': 'solid', '': 'solid'} # used default color if no delta

    cmap = cm.ScalarMappable(norm = colors.Normalize(vmin=0., vmax=max_scale),
                             cmap = cm.rainbow)
    link_colors = dict()
    if options.paths:
        paths_file = "../../expData/"+network_name+"/paths/"+solver+"_0"
        link_colors = get_links_in_path(paths_file)

    for src, dst, data in G.edges(data=True):
        #print src, dst, G.node[src], G.node[dst], data
        # edge color
        link_score = 0
        num_paths = 1
        if options.paths:
            link = '(s'+str(src)+',s'+str(dst)+')'
            rlink = '(s'+str(dst)+',s'+str(src)+')'
            if link in link_colors:
                data['edge_colors'] = link_colors[link]
            if rlink in link_colors:
                data['edge_colors'] = data.get('edge_colors', []) + link_colors[rlink]

        else:
            link = '(s'+str(src)+',s'+str(dst)+')'
            rlink = '(s'+str(dst)+',s'+str(src)+')'
            link_max_cong = max(get_link_congestion(exputils, solver, link))
            rlink_max_cong = max(get_link_congestion(exputils, solver, rlink))
            link_score = max(link_max_cong, rlink_max_cong) # take max of links in either dir
            data['edge_color'] = cmap.to_rgba(link_score)
            data['edge_width'] = 20*link_score/max_scale+1

        lon1 = G.node[src]['Longitude']
        lat1 = G.node[src]['Latitude']
        lon2 = G.node[dst]['Longitude']
        lat2 = G.node[dst]['Latitude']

        # Set color based on normalised link utilization
        if 'edge_color' in data:
            edge_color = data['edge_color']
        else:
            edge_color = default_edge_color

        linestyle = 'solid'
        if data.get('inferred'):
            linestyle = 'dotted'
        if options.paths:
            linestyle = 'dashed'

        if 'edge_width' in data:
            curr_line_width = line_width * int(data['edge_width'])
        else:
            curr_line_width = line_width

        if 'zorder' in data:
            zorder = data['zorder']
        else:
            zorder = 1/(1+curr_line_width) + 1

        if (lat1 == lat2) and (lon1 == lon2):
            # Case of same location, eg Perth1 Perth2
            continue

        # Generate small set of points to use for wrap-around check
        (x, y) = m.gcpoints(lon1, lat1, lon2, lat2, 15)
        #TODO Look how great circle decides how to go across or wrap around
        # and use this instead of lon1 lon2 comparison

        # Matplotlib can't handle wrap around of co-ords
        # Monotonicity check for wrap around eg
        # [32060855, 32615246, -2674010, -2101788]

        x_diff = np.diff(x)
        if np.all(x_diff > 0) or np.all(x_diff < 0) or np.all(x_diff == 0):
            m.drawgreatcircle(lon1, lat1, lon2, lat2, color = edge_color,
                          linewidth=curr_line_width,
                          alpha = 0.7,
                          linestyle = linestyle,
                          #dashes=(4,1),
                          zorder=zorder,
                          solid_capstyle="round")
            if options.paths:
                edge_colors = data.get('edge_colors',[])
                curr_line_width = 6
                for edge_color in edge_colors:
                    offset = get_offset(edge_color)
                    print edge_color
                    m.drawgreatcircle(lon1, lat1+offset/3, lon2, lat2+offset/3, color = edge_color,
                              linewidth=2*curr_line_width,
                              alpha = 0.9,
                              linestyle = '--',
                              #dashes=(4,1),
                              zorder=zorder+1)


        else:
            # Generate larger set of points to plot with
            (x, y) = m.gcpoints(lon1, lat1, lon2, lat2, 50)
            # Recalculate diff for new (bigger) set of co-ords
            x_diff = np.diff(x)

            # Work out if (aside from break point) increasing or decreasing
            # This changes the comparison to find the breakpoint
            if lon1 < lon2:
                # decreasing
                break_index = np.nonzero( x_diff > 0)[0]
            else:
                # increasing
                break_index = np.nonzero( x_diff < 0)[0]
            # Compensate for the diff operation shifting values left one
            break_index += 1
            # TODO: could check that the highest in first half, and lowest
            #in second half are close enough to the boundaries, if not then
            #interpolate or increasing the number of points otherwise get
            #big gap in the line
            # Plot either side of this index
            #
            #TODO work out why internode cuts off across pacific
            # - may need to interpolate
            #edge_color = 'r'
            if len(break_index) > 1:
                print "Error: edge crosses map boundary more than once"
                continue
            m.plot(x[:break_index],y[:break_index], color = edge_color,
                   linewidth = curr_line_width,
                   linestyle=linestyle, zorder=zorder)
            m.plot(x[break_index:],y[break_index:], color = edge_color,
                   linewidth = curr_line_width,
                   linestyle=linestyle, zorder=zorder)

    #plot delta colors
    delta_colors = {
            'added': '#339966', 'removed': 'r', 'modified': '#3366ff',
            '': node_color} # used default color if no delta
    node_deltas = dict( (n, d.get("delta")) for n,d in G.nodes(data=True))
    if len(node_deltas):
        # unchanged
        plotted_nodes = nx.draw_networkx_nodes(G, pos,
                nodelist = [n for n, d in node_deltas.items() if not d ],
                node_size = node_size,
                #alpha = 0.8,
                linewidths = (0,0),
                node_shape = 'o',
                zorder = delta_zorders[''],
                node_color = node_color)

        # added
        plotted_nodes = nx.draw_networkx_nodes(G, pos,
                nodelist = [n for n, d in node_deltas.items() if d == "added"],
                node_size = node_size,
                #alpha = 0.8,
                linewidths = (0,0),
                node_shape = 'd',
                zorder = delta_zorders['added'],
                node_color = delta_colors['added'])

        # removed
        plotted_nodes = nx.draw_networkx_nodes(G, pos,
                nodelist = [n for n, d in node_deltas.items() if d == "removed"],
                node_size = node_size,
                #alpha = 0.8,
                linewidths = (0,0),
                zorder = delta_zorders['removed'],
                node_shape = '^',
                node_color = delta_colors['removed'])

        # modified
        plotted_nodes = nx.draw_networkx_nodes(G, pos,
                nodelist = [n for n, d in node_deltas.items() if d == "modified"],
                node_size = node_size,
                #alpha = 0.8,
                linewidths = (0,0),
                node_shape = 's',
                zorder = delta_zorders['modified'],
                node_color = delta_colors['modified'])

    else:
        plotted_nodes = nx.draw_networkx_nodes(G, pos,
                nodelist = geocoded_cities,
                node_size = node_size,
                #alpha = 0.8,
                linewidths = (0,0),
                node_color = node_color)



        nx.draw_networkx_nodes(G, pos, nodelist = hyperedge_nodes,
                                node_size = node_size,
                                #alpha = 0.8,
                                linewidths = (0,0),
                                node_color = node_color,
                                node_shape='d',)

        if external_node_scale:
            nx.draw_networkx_nodes(G, pos, nodelist = external_nodes,
                                node_size = node_size,
                                #alpha = 0.8,
                                linewidths = (0,0),
                                node_color = 'r',
                                node_shape='^',)

        nx.draw_networkx_nodes(G, pos, nodelist = inferred_nodes,
                            node_size = node_size,
                            #alpha = 0.8,
                            linewidths = (0,0),
                            node_color = node_color,
                            node_shape='s')

    if use_labels:
        label_pos = dict()
        for k,v in pos.iteritems():
            if nx.get_node_attributes(G,'label')[k] == 'Seattle':
                label_pos[k] = (v[0]+1000000,v[1])
            elif nx.get_node_attributes(G,'label')[k] == 'Miami':
                label_pos[k] = (v[0]-700000,v[1]-300000)
            elif nx.get_node_attributes(G,'label')[k] == 'Halifax':
                label_pos[k] = (v[0]+1200000,v[1]-300000)
            else:
                label_pos[k] = (v[0]+1000000,v[1]-500000)

        nx.draw_networkx_labels(G, label_pos,
                            labels=labels,
                            font_size = label_font_size,
                            font_color = font_color,
                           )

    if edge_label_attribute:
        edge_labels = {}
        for s, t, d in G.edges_iter(data=True):
            if edge_label_attribute in d:
                edge_labels[(s,t)] = d[edge_label_attribute]
            else:
                # blank entry
                edge_labels[(s,t)] = ""

        # NetworkX will automatically put a box behind label, make invisible
        # by setting alpha to zero
        bbox = dict(boxstyle='round',
                    ec=(1.0, 1.0, 1.0, 0),
                    fc=(1.0, 1.0, 1.0, 0.0),
                    )

        nx.draw_networkx_edge_labels(G, pos, edge_labels, font_size=edge_font_size,
                                    bbox = bbox)

        """
        nx.draw_networkx_edges(g_outliers, pos, labels = labels,
                            with_labels = False,
                            arrows = False, alpha = 1, width = 0.3,
                            linewidths = (0,0), font_weight = "bold",
                            font_size = 10, font_color = "w",
                            edge_color ='#336699')
                                    """
    # Add title
    if title:
        network_title = ax.text(0.98, 0.98,
                G.graph['Network'],
                horizontalalignment='right',
                #weight='heavy',
                fontsize=16, color=title_color,
                verticalalignment='top',
                transform=ax.transAxes)

    if options.showalg:
        ax.text(0.02, 0.02,
                CommonConf.gen_label(solver),
                style='normal',
                horizontalalignment='left',
                fontsize=max(label_font_size,16), color=title_color,
                verticalalignment='top',
                transform=ax.transAxes)

    # Show colorbar for edge color encoding
    if options.link_util:
        a = np.array([[0,max_scale]])
        im = plt.imshow(a, cmap=cm.rainbow)
        ax = plt.gca()
        divider = make_axes_locatable(ax)
        cax = divider.append_axes("right", size="2%", pad=0.02)
        plt.colorbar(im, cax=cax)

    plt_file_pdf = open(plt_filename_pdf, "w")

    plt.savefig( plt_file_pdf, format = 'pdf',
                bbox_inches='tight',
                facecolor = "w", dpi = 300,
                pad_inches=0,
            )

    if show_figure:
        plt.show()

    if not standalone:
        plt.close()


def main():

    network_files = []

    if options.file:
        network_files.append(options.file)
        path, filename = os.path.split(options.file)

    if len(network_files) == 0:
        print "No files found. Please specify a -f file"
        sys.exit(0)

    if options.paths:
        options.link_util = False
    else:
        options.link_util = True

    # Don't want to write into root directory
    path = os.path.abspath(path)

    if options.output_dir:
        output_path = options.output_dir
    else:
        output_path = path + "/geoplot"
        # And append the map type to the directory
        if options.link_util:
            if options.bluemarble:
                output_path += "_bm"
            elif options.back_image:
                output_path += "_img"
            else:
                output_path += "_flat"
        else:
            output_path += "_path"

    if not os.path.isdir(output_path):
        os.mkdir(output_path)

    for index, net_file in enumerate(sorted(network_files)):
        # Extract name of network from file path
        path, filename = os.path.split(net_file)
        network_name, extension = os.path.splitext(filename)
        print "Plotting: %s (%s/%s)"%(network_name,
                                      # Index starts at 0, offset else get 0/1
                                      index+1,
                                      len(network_files))
        print "reading %s " % net_file

        if options.max_scale == 10:
            if options.paths:
                options.max_scale = 3
            else:
                options.max_scale = get_max_exp_utilization(network_name)


        for solver in ['ecmp', 'edksp', 'ksp', 'raeke', 'mcf', 'vlb',
                       'semimcfraeke', 'semimcfksp', 'spf']:
            G = nx.read_gml(net_file, label='id')

            # Reduce to undirected single edge graph for simplicity
            G = nx.Graph(G)
            G.name = network_name

            plot_graph(G, solver, output_path,
                       title=options.title,
                       use_bluemarble=options.bluemarble,
                       back_image = options.back_image,
                       expand_scale = options.expand_scale,
                       max_scale = options.max_scale,
                       use_labels=options.labels,
                       edge_label_attribute=options.edge_label,
                       external_node_scale=options.external_node_scale,
                       numeric_labels = options.numeric_labels,
                       edge_label_order = options.edge_label_order,
                       basemap_resolution_level = options.res,
                       node_size = options.node_size,
                       line_width = options.line_width,
                       manual_image_scale = options.image_scale,
                       label_font_size = options.label_font_size,
                       no_update = options.no_update,
                       edge_colormap = options.edge_colormap,
                       country_color = options.country_color,
                       standalone = False,
                )

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
