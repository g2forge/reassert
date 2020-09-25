package com.g2forge.reassert.contract;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.type.MapLikeType;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.dataformat.csv.CsvSchema;
import com.fasterxml.jackson.dataformat.csv.CsvSchema.ColumnType;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.contract.IContractIdentified;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.ITermsLoader;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.terms.Terms;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class CSVTermsLoader<T> implements ITermsLoader {
	public static class TermsMapper {
		@Getter
		protected static class Entry<C, T> {
			protected C contract;

			@JsonIgnore
			protected Map<T, TermRelation> terms;

			@JsonAnySetter
			public void setTerm(T term, TermRelation relation) {
				if (terms == null) terms = new LinkedHashMap<>();
				terms.put(term, relation);
			}
		}

		@Getter(lazy = true, value = AccessLevel.PROTECTED)
		private static final CsvMapper mapper = computeMapper();

		protected static CsvMapper computeMapper() {
			final CsvMapper mapper = new CsvMapper();
			mapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);
			mapper.registerModule(new ParanamerModule());
			return mapper;
		}

		public <C, T> Map<C, ITerms<T>> read(Class<C> contractClass, Class<? extends T> termClass, IDataSource source) {
			final CsvMapper mapper = getMapper();
			final JavaType type = mapper.getTypeFactory().constructParametricType(Entry.class, contractClass, termClass);
			final ObjectReader reader = mapper.readerFor(type).with(mapper.schemaFor(type).withHeader().withColumnReordering(true));

			final List<Entry<C, T>> entries;
			try (final InputStream stream = source.getStream(ITypeRef.of(InputStream.class))) {
				entries = reader.<Entry<C, T>>readValues(stream).readAll();
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
			return entries.stream().collect(Collectors.toMap(Entry::getContract, e -> new Terms<>(e.getTerms())));
		}

		public <C, T> void write(IDataSink sink, Map<? super C, ? extends ITerms<? super T>> contracts) {
			// Map the contracts into a generic data structure (list of maps, one map per row)
			final List<Map<Object, Object>> collection = contracts.entrySet().stream().map(entry -> {
				final Map<Object, Object> retVal = new LinkedHashMap<>();
				retVal.put("contract", entry.getKey());
				retVal.putAll(entry.getValue().getRelations(false));
				return retVal;
			}).collect(Collectors.toList());

			final CsvSchema.Builder schemaBuilder = CsvSchema.builder();
			final LinkedHashSet<String> columns = collection.stream().flatMap(e -> e.keySet().stream()).map(Object::toString).collect(Collectors.toCollection(LinkedHashSet::new));
			schemaBuilder.addColumns(columns, ColumnType.STRING);
			final CsvSchema schema = schemaBuilder.build().withHeader().withColumnReordering(true);

			final CsvMapper mapper = getMapper();
			final MapLikeType type = mapper.getTypeFactory().constructMapLikeType(Map.class, Object.class, Object.class);
			final ObjectWriter writer = mapper.writerFor(type).with(schema);
			try (final OutputStream stream = sink.getStream(ITypeRef.of(OutputStream.class))) {
				writer.writeValues(stream).writeAll(collection);
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}
	}

	protected final Class<?> resourceClass;

	protected final Class<? extends T> termClass;

	protected final Terms<T> EMPTY_TERMS = Terms.<T>builder().build();

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<String, ITerms<T>> allTerms = computeAllTerms();

	protected Map<String, ITerms<T>> computeAllTerms() {
		final Class<?> klass = getResourceClass();
		final ResourceDataSource source = new ResourceDataSource(new Resource(klass, klass.getSimpleName().toLowerCase() + ".csv"));
		return new TermsMapper().<String, T>read(String.class, getTermClass(), source);
	}

	@Override
	public <_T> ITerms<_T> getTerms(IContractIdentified contract, Class<? extends _T> termClass) {
		if (!termClass.isAssignableFrom(getTermClass())) return null;

		final ITerms<T> terms = getAllTerms().getOrDefault(contract.getShortID(), EMPTY_TERMS);
		@SuppressWarnings("unchecked")
		final ITerms<_T> retVal = (ITerms<_T>) terms;
		return retVal;
	}
}
