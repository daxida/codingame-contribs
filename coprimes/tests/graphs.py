import matplotlib.pyplot as plt
import networkx as nx
from collections import defaultdict
from utils import get_primes_iter, get_prime_factors

'''
Coprime graph: graph from a sequence xs such that the vertices
are the elements of xs, and a and b are connected iif a and b are coprimes

Given a graph, builds a sequence whose coprime graph is the given graph.

The purpose is to build interesting sequences from well known graphs.
'''


def plot(g, labels):
    pos = nx.spring_layout(g)
    nx.draw_networkx_nodes(g, pos)
    nx.draw_networkx_labels(g, pos, labels=labels)
    nx.draw_networkx_edges(g, pos, arrows=False)  # arrows if directed
    plt.show()


def labelling(g, primes):
    # Takes a (networkx.classes.graph.Graph) and builds its coprime label.
    # The label of a vertex u is either:
    #   - a number
    #   - a prime decomposition of that number (easier visualization)
    # such that:
    #   there's an edge between nodes u and v iif u and v are coprime

    graph = nx.to_dict_of_dicts(g)
    n = len(graph)

    degrees = {node: len(graph[node]) for node in graph}
    labels = {node: 1 for node in graph}

    # Keeps track of labelled edges so to not unnecessarily
    # multiply the labels by more primes that we need.
    # So if we multiplied the edge (u,v) we don't need
    # to do it again with (v, u)
    labelled = defaultdict(bool)

    for d in range(n):
        for node_1 in graph:
            if not degrees[node_1] == d:
                continue

            for node_2 in graph:
                if node_1 == node_2:
                    continue

                # If they are not neighbours, then they share a prime factor
                if node_2 not in graph[node_1]:
                    if not labelled[(node_1, node_2)]:
                        p = next(primes)
                        labels[node_1] *= p
                        labels[node_2] *= p
                        labelled[(node_1, node_2)] = True
                        labelled[(node_2, node_1)] = True

    seq = list(labels.values())
    print("Sequence", seq)

    # labels = {k:(k,get_prime_factors(v)) for k,v in labels.items()}
    labels = {k: get_prime_factors(v) for k, v in labels.items()}

    return labels


def test(g, primes):
    graph = nx.to_dict_of_dicts(g)
    labels = labelling(g, primes)
    plot(g, labels)


def main():
    # Interesting atlas graphs

    # 400
    primes = get_primes_iter(10000)

    # g = nx.graph_atlas(400)
    # g = nx.cubical_graph() # N = 8
    # g = nx.truncated_tetrahedron_graph() # Archimedean N = 12
    # g = nx,moebius_kantor_graph() # N = 16

    g = nx.heawood_graph()  # N = 14

    test(g, primes)


main()
