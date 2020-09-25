package com.g2forge.reassert.contract;

import java.util.Map;

import org.junit.Test;

import com.g2forge.alexandria.adt.associative.map.MapBuilder;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.CSVTermsLoader.TermsMapper;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

public class TestCSVTermsLoader {
	@Test
	public void read() {
		final Map<String, ITerms<String>> terms = new TermsMapper().read(String.class, String.class, new ResourceDataSource(new Resource(getClass(), "test.csv")));
		HAssert.assertEquals(HCollection.asSet("TestContract"), terms.keySet());
		HAssert.assertEquals(Terms.<String>builder().include("A").exclude("B").build(), terms.get("TestContract"));
	}
	
	@Test
	public void write() {
		final Map<String, ITerms<String>> contracts = new MapBuilder<String, ITerms<String>>().put("TestContract", Terms.<String>builder().include("A").exclude("B").unspecified("C").build()).build();
		final ByteArrayDataSink sink = new ByteArrayDataSink();
		new TermsMapper().write(sink, contracts);
		HAssert.assertEquals(new Resource(getClass(), "test.csv"), sink.getStream().toString());
	}
}
