package com.g2forge.reassert.git;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
@JsonIgnoreProperties("material")
public class GitCoordinates implements ICoordinates {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@JsonIgnore
	protected final GitSystem system;

	protected final String url;
	
	protected final String branch;
}
