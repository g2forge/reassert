package com.g2forge.reassert.maven;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.test.ATestSystem;
import com.g2forge.reassert.maven.model.MavenPackaging;

import lombok.Getter;

public class TestMavenCoordinates extends ATestSystem {
	protected static final String GROUPID = "com.g2forge.reassert";

	protected static final String ARTIFACTID = "re-maven";

	protected static final String VERSION = "0.0.1-SNAPSHOT";

	@Getter(lazy = true)
	private final MavenSystem system = new MavenSystem(getContext());

	@Test
	public void builder() {
		HAssert.assertEquals(new MavenCoordinates(getSystem(), GROUPID, ARTIFACTID, VERSION, MavenPackaging.JAR), MavenCoordinates.builder().groupId(GROUPID).artifactId(ARTIFACTID).version(VERSION).build());
	}

	@Test
	public void equality() {
		HAssert.assertEquals(new MavenCoordinates(getSystem(), GROUPID, ARTIFACTID, VERSION.toUpperCase(), MavenPackaging.JAR), new MavenCoordinates(getSystem(), GROUPID, ARTIFACTID, VERSION.toLowerCase(), MavenPackaging.JAR));
	}

	@Test
	public void system() {
		final ICoordinates coordinates = new MavenCoordinates(getSystem(), GROUPID, ARTIFACTID, VERSION, MavenPackaging.JAR);
		HAssert.assertTrue(coordinates.getSystem().isValid(coordinates));
	}
}
