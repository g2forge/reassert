package com.g2forge.reassert.list;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.system.ARepository;
import com.g2forge.reassert.core.model.artifact.Artifact;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class AListRepository extends ARepository<ListCoordinates, ListSystem> {
	protected static <Entry> List<Entry> read(ListCoordinates coordinates, final Class<Entry> type) {
		final CsvMapper mapper = new CsvMapper();
		final ObjectReader reader = mapper.readerFor(type).with(mapper.schemaFor(type).withHeader().withColumnReordering(true));
		try (final InputStream stream = coordinates.getSource().getStream(ITypeRef.of(InputStream.class))) {
			return reader.<Entry>readValues(stream).readAll();
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	@Override
	public ListSystem getSystem() {
		return ListSystem.create();
	}

	protected abstract void internal(ListCoordinates coordinates, IReassertGraphBuilder builder);

	@Override
	public Artifact<ListCoordinates> load(ListCoordinates coordinates, IReassertGraphBuilder builder) {
		assertValid(coordinates);
		internal(coordinates, builder);
		return null;
	}
}
