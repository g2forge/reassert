package com.g2forge.reassert.contract.terms;

import java.util.Map;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Terms;

public class TestTermsLoader {
	@Test
	public void test() {
		final Map<String, ITerms<String>> terms = new TermsLoader().load(String.class, String.class, new ResourceDataSource(new Resource(getClass(), "test.csv")));
		HAssert.assertEquals(HCollection.asSet("TestContract"), terms.keySet());
		HAssert.assertEquals(Terms.<String>builder().include("A").exclude("B").build(), terms.get("TestContract"));
	}
}
