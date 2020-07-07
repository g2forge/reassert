package com.g2forge.reassert.reassert;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.git.GitCoordinates;
import com.g2forge.reassert.git.GitSystem;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.MavenSystem;
import com.g2forge.reassert.maven.model.MavenPackaging;

public class TestReassertContext {
	protected static final String GROUPID = "com.g2forge.reassert";

	protected static final String ARTIFACTID = "re-assert";

	protected static final String VERSION = "0.0.1-SNAPSHOT";

	protected static final String URL = "git@github.com:g2forge/reassert.git";

	protected static void hasValidSystem(final ICoordinates coordinates) {
		HAssert.assertTrue(ReassertContext.getContext().getSystems().stream().filter(s -> s.isValid(coordinates)).findAny().isPresent());
	}

	@Test
	public void git() {
		final GitSystem system = ReassertContext.getContext().findSystem(GitSystem.class);
		hasValidSystem(new GitCoordinates(system, URL, null));
	}

	@Test
	public void maven() {
		final MavenSystem system = ReassertContext.getContext().findSystem(MavenSystem.class);
		hasValidSystem(new MavenCoordinates(system, GROUPID, ARTIFACTID, VERSION, MavenPackaging.JAR));
	}
}
