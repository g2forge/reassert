package com.g2forge.reassert.reassert.summary;

import java.io.IOException;
import java.io.OutputStream;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
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
	
	public void renderArtifacts(ReportSummary reportSummary, IDataSink sink) {
		final CsvMapper mapper = new CsvMapper();
		mapper.disable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
		mapper.registerModule(new SummaryModule(new ReassertVertexDescriber(getContext())));
		
		final ObjectWriter writer = mapper.writerFor(ArtifactSummary.class).with(mapper.schemaFor(ArtifactSummary.class).withHeader().withColumnReordering(true));
		try (final OutputStream stream = sink.getStream(ITypeRef.of(OutputStream.class))) {
			writer.writeValues(stream).writeAll(reportSummary.getArtifacts());
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	public ReportSummary summarize(IReport report) {
		return null;
	}
}
