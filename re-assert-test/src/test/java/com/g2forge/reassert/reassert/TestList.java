package com.g2forge.reassert.reassert;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.list.ListRepository;
import com.g2forge.reassert.reassert.ReassertContext;

import lombok.AccessLevel;
import lombok.Getter;

public class TestList {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TestGraph exampleGraph = computeExampleGraph();

	protected static TestGraph computeExampleGraph() {
		final IContext context = ReassertContext.getContext();
		final ListRepository repository = new ListRepository(context, new ReassertVertexDescriber(context));
		return new TestGraph(new Artifact<>(repository, new ListCoordinates(new ResourceDataSource(new Resource(TestList.class, "list.json")))), HCollection.emptyList());
	}

	@Test
	public void visualize() {
		HAssert.assertEquals(new Resource(getClass(), "list.dot"), TestVisualizer.create().visualize(getExampleGraph().getGraph()));
	}
}
