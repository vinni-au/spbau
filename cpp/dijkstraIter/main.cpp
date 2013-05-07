#include <utility>
#include <vector>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>

using std::pair;
using std::vector;

using boost::adjacency_list;
using boost::vecS;
using boost::directedS;
using boost::vertex;
using boost::vertex_index;
using boost::get;
using boost::weight_map;
using boost::make_iterator_property_map;
using boost::graph_traits;

using boost::dijkstra_shortest_paths;

typedef long long edge_len_t;
typedef long long vertex_weight_t;

struct _edge_p_t {
  edge_len_t l_;
};

struct _vertex_p_t {
  _vertex_p_t(vertex_weight_t w) : w_(w) {}
  vertex_weight_t w_;
};

typedef adjacency_list<
          vecS, vecS, directedS, _edge_p_t, _vertex_p_t
        > graph_t;

typedef graph_traits<graph_t>::vertex_descriptor vertex_desc_t;

typedef
  enum {
    A, B, C, D, E,
    v_count
  } vertex_t;

typedef pair<vertex_t, vertex_t> edge_t;


edge_t edges[] = {
  edge_t(A, C), edge_t(B, B),
  edge_t(B, D), edge_t(B, E),
  edge_t(C, B), edge_t(C, D),
  edge_t(D, E), edge_t(E, A),
  edge_t(E, B)
};

edge_len_t e_weights[] = {
  1, 2, 3, 4, 5, 6, 7, 8, 9
};





int main(int ARGC, char *ARGV[])
{
  size_t e_count = sizeof(edges) / sizeof(edge_t);

  graph_t g(edges, edges + e_count, e_weights, v_count);

  vector<edge_len_t> ps;
  vector<vertex_desc_t> ds;
  dijkstra_shortest_paths(
    g,
    vertex(A, g),
    weight_map(get(&_edge_p_t::l_, g))
      .distance_map(ds.data())
      .predecessor_map(ps.data())
  );

  return 0;
}
