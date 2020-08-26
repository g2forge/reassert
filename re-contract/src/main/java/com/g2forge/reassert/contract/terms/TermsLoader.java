package com.g2forge.reassert.contract.terms;

import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.contract.Terms;

import lombok.Getter;

public class TermsLoader {
	@Getter
	public static class Entry<C, T> {
		protected C contract;

		protected Map<T, TermRelation> terms;
		
		@JsonAnySetter 
		public void terms(T term, TermRelation relation) {
			if (terms == null) terms = new LinkedHashMap<>();
		    terms.put(term, relation);
		}
	}

	public <C, T> Map<C, ITerms<T>> load(Class<C> contractClass, Class<T> termClass, IDataSource source) {
		final List<Entry<C, T>> entries;

		{
			final CsvMapper mapper = new CsvMapper();
			mapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);
			mapper.registerModule(new ParanamerModule());

			final JavaType type = mapper.getTypeFactory().constructParametricType(Entry.class, contractClass, termClass);

			final ObjectReader reader = mapper.readerFor(type).with(mapper.schemaFor(type).withHeader().withColumnReordering(true));
			try (final InputStream stream = source.getStream(ITypeRef.of(InputStream.class))) {
				entries = reader.<Entry<C, T>>readValues(stream).readAll();
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		return entries.stream().collect(Collectors.toMap(Entry::getContract, e -> new Terms<>(e.getTerms())));
	}
}
