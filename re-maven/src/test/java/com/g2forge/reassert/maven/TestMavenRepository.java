package com.g2forge.reassert.maven;

import java.util.Collection;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.enigma.diagram.dot.convert.DotRenderer;
import com.g2forge.reassert.core.algorithm.ReassertGraphVisualizer;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.file.Describes;
import com.g2forge.reassert.core.test.ATestRepository;
import com.g2forge.reassert.license.StandardLicense;
import com.g2forge.reassert.maven.model.MavenLicense;
import com.g2forge.reassert.maven.model.MavenPOM;
import com.g2forge.reassert.maven.model.MavenPackaging;

import lombok.Getter;

public class TestMavenRepository extends ATestRepository<MavenCoordinates> {
	@Getter(lazy = true)
	private final MavenSystem system = getContext().findSystem(MavenSystem.class);

	public Context.ContextBuilder computeContext(Context.ContextBuilder builder) {
		return super.computeContext(builder).module(MavenModule.create());
	}

	@Override
	protected MavenCoordinates createCoordinates() {
		return new MavenCoordinates(getSystem(), "com.g2forge.alexandria", "ax-root", "0.0.13", null);
	}

	@Test
	public void licenses() {
		final Graph<IVertex, IEdge> graph = getGraph();
		final Artifact<MavenCoordinates> artifact = HReassertModel.findArtifact(graph, getCoordinates());
		final Collection<ILicense> licenses = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicense.class));
		HAssert.assertEquals(HCollection.asList(StandardLicense.Apache2), licenses);
	}

	@Test
	public void load() {
		final Graph<IVertex, IEdge> graph = getGraph();
		final Artifact<MavenCoordinates> artifact = HReassertModel.findArtifact(graph, getCoordinates());
		HAssert.assertEquals(getCoordinates(), artifact.getCoordinates());
	}

	@Test
	public void pom() {
		final Graph<IVertex, IEdge> graph = getGraph();
		final Artifact<MavenCoordinates> artifact = HReassertModel.findArtifact(graph, getCoordinates());
		final Collection<MavenPOM> poms = HReassertModel.get(graph, artifact, false, Describes.class::isInstance, ITypeRef.of(MavenPOM.class)).stream().map(pom -> pom.toBuilder().clearDependencies().clearProperties().build()).collect(Collectors.toList());
		HAssert.assertEquals(HCollection.asList(MavenPOM.builder().coordinates(getCoordinates().toBuilder().packaging(MavenPackaging.POM).build()).license(new MavenLicense("The Apache License, Version 2.0", "https://github.com/${alexandria.organization}/${alexandria.repository}/blob/${project.version}/LICENSE")).build()), poms);
	}

	@Test
	public void visualize() {
		final String actual = new DotRenderer().render(new ReassertGraphVisualizer(getContext()).visualize(getGraph()));
		HAssert.assertEquals(new Resource(getClass(), "visualize.dot"), actual);
	}
}
