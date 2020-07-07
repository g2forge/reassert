package com.g2forge.reassert.reassert.algorithm.work;

import java.util.Collection;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.license.StandardWorkTypeFactory;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.reassert.algorithm.ATestVisitor;
import com.g2forge.reassert.reassert.algorithm.ReassertWorkVisitor;
import com.g2forge.reassert.reassert.algorithm.example.ExampleGraph;
import com.g2forge.reassert.reassert.model.findings.UnknownWorkTypeFinding;

public class TestReassertWorkVisitor extends ATestVisitor {
	@Test
	public void bsd3() {
		test("bsd3");
	}

	@Test
	public void gpl() {
		test("gpl");
	}

	@Override
	protected ExampleGraph load(final Artifact<ListCoordinates> artifact) {
		return new ExampleGraph(artifact, new ReassertWorkVisitor(StandardWorkTypeFactory.create()));
	}

	@Test
	public void unknown() {
		final Collection<IFinding> findings = load("unknown").getReport().getFindings();
		HAssert.assertEquals(1, findings.size());
		HAssert.assertInstanceOf(UnknownWorkTypeFinding.class, HCollection.getOne(findings).getInnermostFinding());
	}
}
