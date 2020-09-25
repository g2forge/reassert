package com.g2forge.reassert.core.model.report;

import java.util.Collection;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Report implements IReport {
	public static class ReportBuilder implements IFindingConsumer, IBuilder<Report> {
		@Override
		public void found(Collection<? extends IFinding> findings, Collection<? extends IVertex> vertices) {
			for (IFinding finding : findings) {
				finding(finding);
			}
		}

		@Override
		public IVertex found(IFinding finding, Collection<? extends IVertex> vertices) {
			finding(finding);
			return null;
		}
	}

	@Singular
	protected final Collection<IFinding> findings;

	protected final Graph<IVertex, IEdge> graph;

	protected final IOrigins origins;
}
