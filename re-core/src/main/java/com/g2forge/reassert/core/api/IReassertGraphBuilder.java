package com.g2forge.reassert.core.api;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public interface IReassertGraphBuilder {
	public interface ICallback<Coordinates extends ICoordinates> {
		public void callback(Artifact<Coordinates> artifact, IReassertGraphBuilder builder);

		public Artifact<Coordinates> getArtifact();
	}

	@Getter
	@RequiredArgsConstructor
	public static class RelatedCallback<Coordinates extends ICoordinates> implements ICallback<Coordinates> {
		protected final Artifact<Coordinates> target;

		protected final IEdge edge;

		protected final Artifact<Coordinates> source;

		public RelatedCallback(Artifact<Coordinates> target) {
			this(target, null, null);
		}

		@Override
		public void callback(Artifact<Coordinates> artifact, IReassertGraphBuilder builder) {
			if ((edge != null) && (source != null)) builder.edge(source, target, edge);
		}

		@Override
		public Artifact<Coordinates> getArtifact() {
			return target;
		}
	}

	public default <Coordinates extends ICoordinates> IReassertGraphBuilder callback(Artifact<Coordinates> target) {
		return callback(new RelatedCallback<>(target));
	}

	public default <Coordinates extends ICoordinates> IReassertGraphBuilder callback(Artifact<Coordinates> target, IEdge edge, Artifact<Coordinates> source) {
		return callback(new RelatedCallback<Coordinates>(target, edge, source));
	}

	public IReassertGraphBuilder callback(ICallback<?> callback);

	public IReassertGraphBuilder edge(IVertex source, IVertex target, IEdge edge);

	public IReassertGraphBuilder vertex(IVertex vertex);
}
