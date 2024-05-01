package com.g2forge.reassert.maven.model;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.MavenSystem;

public class TestMavenPOM {
	@Test
	public void parseEffectivePom() {
		final MavenSystem mavenSystem = new MavenSystem(Context.getContext());
		final MavenEffectivePOM actual = mavenSystem.parseEffectivePOM(new ResourceDataSource(new Resource(getClass(), "test-effective-pom.xml.txt")));

		HAssert.assertEquals(1, actual.getProjects().size());
		final MavenPOM project = HCollection.getOne(actual.getProjects());

		HAssert.assertEquals(new MavenCoordinates(mavenSystem, "Group", "Artifact", "Version", null), project.getCoordinates());
		HAssert.assertNull(project.getParent());
		HAssert.assertNull(project.getLicenses());
		HAssert.assertNull(project.getProperties());
		HAssert.assertEquals(HCollection.asList(new MavenDependency(new MavenCoordinates(mavenSystem, "Dependencies", "One", "1.0.0", null), null, false), new MavenDependency(new MavenCoordinates(mavenSystem, "Dependencies", "Two", "2.0.0", null), MavenScope.Test, false)), project.getDependencies());
		HAssert.assertEquals(HCollection.asList("ModuleA", "ModuleB"), project.getModules());
		HAssert.assertEquals(HCollection.asList(new MavenProfile("Profile1", null, null, HCollection.asList("ModuleC"))), project.getProfiles());
	}

	@Test
	public void parsePom() {
		final MavenSystem mavenSystem = new MavenSystem(Context.getContext());
		final MavenPOM actual = mavenSystem.parsePOM(new ResourceDataSource(new Resource(getClass(), "test-pom.xml.txt")));
		HAssert.assertEquals(new MavenCoordinates(mavenSystem, "Group", "Artifact", "Version", null), actual.getCoordinates());
		HAssert.assertNull(actual.getParent());
		HAssert.assertNull(actual.getLicenses());
		HAssert.assertNull(actual.getProperties());
		HAssert.assertEquals(HCollection.asList(new MavenDependency(new MavenCoordinates(mavenSystem, "Dependencies", "One", "1.0.0", null), null, false), new MavenDependency(new MavenCoordinates(mavenSystem, "Dependencies", "Two", "2.0.0", null), MavenScope.Test, false)), actual.getDependencies());
		HAssert.assertEquals(HCollection.asList("ModuleA", "ModuleB"), actual.getModules());
		HAssert.assertEquals(HCollection.asList(new MavenProfile("Profile1", null, null, HCollection.asList("ModuleC"))), actual.getProfiles());
	}
}
