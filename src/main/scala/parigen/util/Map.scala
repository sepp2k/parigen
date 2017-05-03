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
}