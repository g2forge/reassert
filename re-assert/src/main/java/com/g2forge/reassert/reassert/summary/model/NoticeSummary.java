package com.g2forge.reassert.reassert.summary.model;

import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class NoticeSummary {
	protected final ICoordinates coordinates;

	protected final ILicense license;
}
