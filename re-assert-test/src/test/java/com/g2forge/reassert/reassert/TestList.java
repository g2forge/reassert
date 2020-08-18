package com.g2forge.reassert.reassert;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;

public class TestList extends ATestFromList {
	@Override
	protected TestGraph load(Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact, HCollection.emptyList());
	}

	@Test
	public void visualize() {
		HAssert.assertEquals(new Resource(getClass(), "list-output.dot"), TestVisualizer.create().visualize(load("list").getGraph()));
	}
}
