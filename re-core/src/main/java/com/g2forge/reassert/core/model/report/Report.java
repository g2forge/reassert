package com.g2forge.reassert.core.model.report;

import java.util.Collection;

import org.jgrapht.Graph;

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
	@Singular
	protected final Collection<IFinding> findings;

	protected final Graph<IVertex, IEdge> graph;
}
