package com.g2forge.reassert.reassert.summary.model;

import java.util.Collection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ReportSummary {
	@Singular
	protected final Collection<ArtifactSummary> artifacts;
	
	@Singular
	protected final Collection<NoticeSummary> notices;
}
