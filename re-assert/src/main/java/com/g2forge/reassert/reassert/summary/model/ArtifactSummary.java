package com.g2forge.reassert.reassert.summary.model;

import java.util.Collection;

import org.jgrapht.GraphPath;
import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.report.IFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ArtifactSummary {
	protected final Level level;

	protected final ICoordinates artifact;

	@Singular
	protected final Collection<IFinding> findings;

	@Singular
	protected final Collection<IUsageApplied> usages;

	@Singular
	protected final Collection<ILicenseApplied> licenses;

	@Singular
	protected final Collection<GraphPath<? extends IVertex, ? extends IEdge>> paths;
}
