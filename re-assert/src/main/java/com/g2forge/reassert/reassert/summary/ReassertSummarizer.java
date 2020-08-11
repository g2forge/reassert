package com.g2forge.reassert.reassert.summary;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
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
		final ReportSummary.ReportSummaryBuilder retVal = ReportSummary.builder();

		final List<Artifact<?>> artifacts = report.getGraph().vertexSet().stream().flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance).sorted(new Comparator<Artifact<?>>() {
			@Getter
			protected final ReassertVertexDescriber describer = new ReassertVertexDescriber(getContext());

			@Override
			public int compare(Artifact<?> o1, Artifact<?> o2) {
				final ReassertVertexDescriber describer = getDescriber();
				final String n1 = describer.apply(o1).getName();
				final String n2 = describer.apply(o2).getName();
				return n1.compareTo(n2);
			}
		}).collect(Collectors.toList());
		for (Artifact<?> artifact : artifacts) {
			final ArtifactSummary.ArtifactSummaryBuilder artifactSummary = ArtifactSummary.builder();
			artifactSummary.artifact(artifact.getCoordinates());
			
			// TODO: Find all the findings, and use that to compute the level

			// Find the license and usage
			artifactSummary.licenses(HReassertModel.get(report.getGraph(), artifact, true, Notice.class::isInstance, ITypeRef.of(ILicense.class)));
			artifactSummary.usages(HReassertModel.get(report.getGraph(), artifact, true, Notice.class::isInstance, ITypeRef.of(IUsage.class)));

			// TODO: Compute paths to this artifact
			
			retVal.artifact(artifactSummary.build());
		}

		return retVal.build();
	}
}
