package com.g2forge.reassert.maven;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.maven.model.MavenPackaging;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
@JsonIgnoreProperties("material")
public class MavenCoordinates implements ICoordinates {
	@JsonIgnore
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final MavenSystem system;

	protected final String groupId;

	protected final String artifactId;

	protected final String version;

	@JsonInclude(Include.NON_NULL)
	@Builder.Default
	protected final MavenPackaging packaging = MavenPackaging.JAR;

	@JsonIgnore
	@ToString.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED, onMethod_ = { @EqualsAndHashCode.Include(replaces = "version") })
	private final String versionLowercase = NullableOptional.ofNullable(getVersion()).map(String::toLowerCase).or(null);
}
