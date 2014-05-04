{application, bctree,
 [{description, "Erlang generic b-tree memory store"},
  {vsn, "0.0.1"},
  {modules, [
  		bctree_memstore,
		bctree_bucket,
		bctree_bucket_sup,
		bctree,
		bctree_item,
		bctree_item_sup,
		bctree_sup,
		bctree_util
	]},
  {registered, [bctree_store, bctree_bucket_sup, bctree_item_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {bctree,[]}}
 ]}.
