package com.g2forge.reassert.cache;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Value {
	protected final String message;

	protected final int index;
}