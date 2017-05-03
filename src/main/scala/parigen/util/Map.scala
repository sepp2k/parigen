package parigen.util

object Map {
    def merge[K, V](map1: Map[K, V], map2: Map[K, V])(mergeFun: (V, V) => V): Map[K, V] = {
        map1 ++ map2.map {
            case (k, v2) =>
                map1.get(k) match {
                    case Some(v1) => k -> mergeFun(v1, v2)
                    case None => k -> v2
                }
        }
    }

    def mergeNestedSetMaps[K1, K2, V](map1: Map[K1, Map[K2, Set[V]]], map2: Map[K1, Map[K2, Set[V]]]): Map[K1, Map[K2, Set[V]]] = {
        merge(map1, map2) {
            (innerMap1, innerMap2) => merge(innerMap1, innerMap2)(_ ++ _)
        }
    }
}