package com.g2forge.reassert.cache;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;

public class TestNoCache {
	@Test
	public void identity() {
		HAssert.assertEquals("Hello", new NoCache().createArea(CacheAreaDescriptor.builder().function((key, path) -> key).build()).apply("Hello"));
	}
}
