package com.g2forge.reassert.reassert.summary;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import org.slf4j.event.Level;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.dataformat.csv.CsvMapper.Builder;
import com.g2forge.alexandria.java.adt.compare.CollectionComparator;
import com.g2forge.alexandria.java.adt.compare.ComparableComparator;
import com.g2forge.alexandria.java.adt.compare.MappedComparator;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.contract.convert.ReportRenderer;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.report.GraphContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.work.Work;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.reassert.summary.convert.ASummaryModule;
import com.g2forge.reassert.reassert.summary.convert.ArtifactsSummaryModule;
import com.g2forge.reassert.reassert.summary.convert.FindingSummarySerializer;
import com.g2forge.reassert.reassert.summary.convert.FindingsSummaryModule;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.FindingSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertSummarizer {
	protected final IContext context;

	protected Set<IVertex> computeOrigins(IReport report, final Graph<IVertex, IEdge> graph) {
		final Set<Artifact<?>> reportOrigins = report.getOrigins().getOrigins().keySet();
		if (graph.vertexSet().containsAll(reportOrigins)) return Collections.unmodifiableSet(reportOrigins);

		final ITypeRef<Artifact<?>> artifactType = new ATypeRef<Artifact<?>>() {};
		return graph.vertexSet().stream().flatMap(artifactType::castIfInstance).filter(a -> !graph.incomingEdgesOf(a).stream().map(graph::getEdgeSource).anyMatch(artifactType::isInstance)).collect(Collectors.toSet());
	}

	protected IFunction1<? super ExplanationMode, ? extends ReportRenderer> createRendererFactory(IContext context) {
		return mode -> new ReportRenderer(mode, context);
	}

	protected List<GraphPath<IVertex, IEdge>> readable(List<GraphPath<IVertex, IEdge>> paths) {
		return paths.stream().filter(path -> {
			final List<IVertex> vertexList = path.getVertexList();
			if (vertexList.size() <= 2) return true;
			return vertexList.subList(1, vertexList.size() - 1).stream().allMatch(v -> !(v instanceof Work));
		}).collect(Collectors.toList());
	}

	protected <T> void render(Class<T> writenType, Class<?> schemaType, Collection<T> value, IDataSink sink, ASummaryModule module) {
		final Builder mapperBuilder = CsvMapper.builder();
		mapperBuilder.disable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
		mapperBuilder.addModule(module);
		final CsvMapper mapper = mapperBuilder.build();

		final ObjectWriter writer = mapper.writerFor(writenType).with(mapper.schemaFor(schemaType).withHeader().withColumnReordering(true).withArrayElementSeparator("\n"));
		try (final OutputStream stream = sink.getStream(ITypeRef.of(OutputStream.class))) {
			writer.writeValues(stream).writeAll(value);
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	public void renderArtifacts(ReportSummary reportSummary, IDataSink sink) {
		final IContext context = getContext();
		render(ArtifactSummary.class, ArtifactSummary.class, reportSummary.getArtifacts(), sink, new ArtifactsSummaryModule(context, createRendererFactory(context)));
	}

	public void renderFindings(ReportSummary reportSummary, IDataSink sink) {
		final IContext context = getContext();
		render(FindingSummary.class, FindingSummarySerializer.StoredFindingSummary.class, reportSummary.getFindings(), sink, new FindingsSummaryModule(context, createRendererFactory(context)));
	}

	public ReportSummary summarize(IReport report) {
		final ReportSummary.ReportSummaryBuilder retVal = ReportSummary.builder();

		final AllDirectedPaths<IVertex, IEdge> allPaths = new AllDirectedPaths<>(report.getGraph());
		final Set<IVertex> origins = computeOrigins(report, report.getGraph());

		final Comparator<IVertex> vertexComparator = new Comparator<IVertex>() {
			@Override
			public int compare(IVertex o1, IVertex o2) {
				final IContext context = getContext();
				final String n1 = context.describe(o1).getName();
				final String n2 = context.describe(o2).getName();
				return n1.compareTo(n2);
			}
		};
		final ITypeRef<Artifact<?>> artifactType = new ATypeRef<Artifact<?>>() {};
		final List<Artifact<?>> artifacts = report.getGraph().vertexSet().stream().flatMap(artifactType::castIfInstance).sorted(vertexComparator).collect(Collectors.toList());
		for (Artifact<?> artifact : artifacts) {
			final ArtifactSummary.ArtifactSummaryBuilder artifactSummary = ArtifactSummary.builder();
			artifactSummary.artifact(artifact.getCoordinates());

			// Find the license and usage
			final Collection<ILicenseApplied> licenses = HReassertModel.get(report.getGraph(), artifact, true, Notice.class::isInstance, ITypeRef.of(ILicenseApplied.class));
			final Collection<IUsageApplied> usages = HReassertModel.get(report.getGraph(), artifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
			artifactSummary.licenses(licenses);
			artifactSummary.usages(usages);

			// Find all the findings, and use that to compute the level
			final Collection<GraphContextFinding> findings = HReassertModel.get(report.getGraph(), artifact, true, Notice.class::isInstance, ITypeRef.of(GraphContextFinding.class));
			if (findings.isEmpty()) artifactSummary.level(Level.INFO);
			else {
				artifactSummary.level(findings.stream().map(IFinding::getLevel).min(ComparableComparator.create()).get());
				for (GraphContextFinding finding : findings) {
					if (finding.getLevel().compareTo(Level.INFO) < 0) artifactSummary.finding(finding.getFinding());
				}
			}

			// Compute paths to this artifact
			if (!origins.contains(artifact)) artifactSummary.paths(allPaths.getAllPaths(origins, HCollection.<IVertex>asSet(artifact), true, Integer.MAX_VALUE));

			retVal.artifact(artifactSummary.build());
		}

		final List<FindingSummary> findings = new ArrayList<>();
		for (GraphContextFinding finding : report.getGraph().vertexSet().stream().flatMap(ITypeRef.of(GraphContextFinding.class)::castIfInstance).collect(Collectors.toList())) {
			final FindingSummary.FindingSummaryBuilder findingSummary = FindingSummary.builder();
			findingSummary.finding(finding.getFinding());

			// If there's a single related artifact, record it
			final Collection<Artifact<?>> related = HReassertModel.get(report.getGraph(), finding, false, Notice.class::isInstance, artifactType);
			if (related.size() == 1) findingSummary.artifact(HCollection.getOne(related).getCoordinates());

			final List<GraphPath<IVertex, IEdge>> paths = allPaths.getAllPaths(origins, HCollection.<IVertex>asSet(finding), true, Integer.MAX_VALUE);
			final List<GraphPath<IVertex, IEdge>> pathsReadable = readable(paths);
			if (!paths.equals(pathsReadable)) {
				final List<List<IVertex>> pathVertices = paths.stream().map(GraphPath::getVertexList).collect(Collectors.toList());
				final List<List<IVertex>> readableVertices = pathsReadable.stream().map(GraphPath::getVertexList).collect(Collectors.toList());
				pathVertices.equals(readableVertices);
			}
			if (pathsReadable.isEmpty()) throw new Error("There was a finding with no apparent relationship to the artifacts you requested we analyze, this is a bug!");
			findingSummary.paths(pathsReadable);
			findings.add(findingSummary.build());
		}
		// Sort the findings by their path from the origins
		Collections.sort(findings, new MappedComparator<>(FindingSummary::getPaths, new CollectionComparator<>(new MappedComparator<>(p -> {
			final List<? extends IVertex> vertexList = p.getVertexList();
			return vertexList.subList(0, vertexList.size() - 1);
		}, new CollectionComparator<>(vertexComparator)))));
		retVal.findings(findings);

		return retVal.build();
	}
}
