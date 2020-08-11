package com.g2forge.reassert.reassert.summary;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.reassert.summary.convert.SummaryModule;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertSummarizer {
	protected final IContext context;

	protected SummaryModule createSummaryModule() {
		return new SummaryModule(getContext());
	}

	public void renderArtifacts(ReportSummary reportSummary, IDataSink sink) {
		final CsvMapper mapper = new CsvMapper();
		mapper.disable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
		mapper.registerModule(createSummaryModule());

		final ObjectWriter writer = mapper.writerFor(ArtifactSummary.class).with(mapper.schemaFor(ArtifactSummary.class).withHeader().withColumnReordering(true).withArrayElementSeparator("\n"));
		try (final OutputStream stream = sink.getStream(ITypeRef.of(OutputStream.class))) {
			writer.writeValues(stream).writeAll(reportSummary.getArtifacts());
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	public ReportSummary summarize(IReport report) {
		final List<Artifact<?>> artifacts = report.getGraph().vertexSet().stream().flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance).collect(Collectors.toList());
		HCollection.sort(artifacts, new Comparator<Artifact<?>>() {
			@Override
			public int compare(Artifact<?> o1, Artifact<?> o2) {
				// TODO Auto-generated method stub
				return 0;
			}
		});
		for (Artifact<?> artifact : artifacts) {

		}
		return null;
	}
}
