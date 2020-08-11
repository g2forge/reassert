package com.g2forge.reassert.reassert;

import java.util.Collection;

import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IOrigins;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;
import lombok.ToString;

@Getter
@RequiredArgsConstructor
@ToString
@Builder
public class Origins implements IOrigins {
	@Singular
	protected final Collection<Artifact<?>> origins;
}