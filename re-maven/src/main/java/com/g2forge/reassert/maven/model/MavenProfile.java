package com.g2forge.reassert.maven.model;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class MavenProfile {
	protected final String id;

	@Singular
	protected final List<MavenDependency> dependencies;

	@Singular
	protected final Map<String, String> properties;

	@Singular
	protected final List<String> modules;
}
