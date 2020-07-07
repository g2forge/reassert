package com.g2forge.reassert.core.model.artifact;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Depends extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = 2370178050989575531L;

	/** Indicates that anyone depending an artifact with this dependency, will inherit the dependency. */
	protected final boolean transitive;

	/** Indicates that this dependency is required at runtime. */
	protected final boolean runtime;

	/** Indicates that this dependency is required during testing. */
	protected final boolean testtime;

	/** Indicates that this dependency is required during compilation. */
	protected final boolean compiletime;

	@Override
	public Depends clone() {
		return toBuilder().build();
	}

	@Override
	public int edgeTypeHashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (compiletime ? 1231 : 1237);
		result = prime * result + (runtime ? 1231 : 1237);
		result = prime * result + (testtime ? 1231 : 1237);
		result = prime * result + (transitive ? 1231 : 1237);
		return result;
	}

	@Override
	public boolean isEdgeTypeEqual(IEdge edge) {
		if (this == edge) return true;
		if (edge == null) return false;
		if (getClass() != edge.getClass()) return false;

		final Depends cast = (Depends) edge;
		if (compiletime != cast.compiletime) return false;
		if (runtime != cast.runtime) return false;
		if (testtime != cast.testtime) return false;
		if (transitive != cast.transitive) return false;
		return true;
	}
}
