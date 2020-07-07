package com.g2forge.reassert.list.model;

import com.g2forge.alexandria.java.adt.identity.IIdentity;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.model.IEdge;

public class EdgeIdentity implements IIdentity<IEdge>, ISingleton {
	protected static final EdgeIdentity INSTANCE = new EdgeIdentity();

	public static EdgeIdentity create() {
		return INSTANCE;
	}

	protected EdgeIdentity() {}

	@Override
	public boolean equals(IEdge _this, Object that) {
		if (!(that instanceof IEdge)) return false;
		return _this.isEdgeTypeEqual((IEdge) that);
	}

	@Override
	public int hashCode(IEdge _this) {
		return _this.edgeTypeHashCode();
	}
}