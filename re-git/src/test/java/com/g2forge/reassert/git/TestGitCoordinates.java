package com.g2forge.reassert.git;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.test.ATestSystem;

import lombok.Getter;

public class TestGitCoordinates extends ATestSystem {
	protected static final String URL = "git@github.com:g2forge/reassert.git";

	@Getter(lazy = true)
	private final GitSystem system = new GitSystem(getContext());

	@Test
	public void equality() {
		HAssert.assertEquals(new GitCoordinates(getSystem(), URL, null), GitCoordinates.builder().url(URL).build());
	}

	@Test
	public void system() {
		final ICoordinates coordinates = new GitCoordinates(getSystem(), URL, null);
		HAssert.assertTrue(coordinates.getSystem().isValid(coordinates));
	}
}
