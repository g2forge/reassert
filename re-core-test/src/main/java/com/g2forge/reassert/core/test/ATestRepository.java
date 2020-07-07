package com.g2forge.reassert.core.test;

import java.util.ArrayList;

import org.jgrapht.Graph;

import com.g2forge.reassert.core.api.ReassertGraphBuilder;
import com.g2forge.reassert.core.api.system.IRepository;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class ATestRepository<Coordinates extends ICoordinates> extends ATestSystem {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Coordinates coordinates = createCoordinates();

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Graph<IVertex, IEdge> graph = computeGraph();

	protected Graph<IVertex, IEdge> computeGraph() {
		final Graph<IVertex, IEdge> graph = HReassertModel.createGraph();
		getRepository().load(getCoordinates(), new ReassertGraphBuilder(graph, new ArrayList<>()));
		return graph;
	}

	protected abstract Coordinates createCoordinates();

	protected IRepository<Coordinates> getRepository() {
		return getSystem().getRepository();
	}

	@Override
	protected abstract ISystem<Coordinates> getSystem();
}
