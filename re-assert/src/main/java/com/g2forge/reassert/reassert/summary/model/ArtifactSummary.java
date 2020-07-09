package com.g2forge.reassert.reassert.summary.model;

import java.util.Collection;
import java.util.List;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
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

	protected final IUsage usage;

	@Singular
	protected final Collection<ILicense> licenses;

	@Singular
	protected final Collection<List<IEdge>> paths;
}
