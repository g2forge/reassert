package com.g2forge.reassert.reassert.summary.model;

import java.util.Collection;

import org.jgrapht.GraphPath;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.report.IFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class FindingSummary {
	protected final IFinding finding;
	
	protected final ICoordinates artifact;

	@Singular
	protected final Collection<GraphPath<? extends IVertex, ? extends IEdge>> paths;
}
