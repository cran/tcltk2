#==============================================================================
# Contains the implementation of the tablelist::sortByColumn and
# tablelist::addToSortColumns commands, as well as of the tablelist sort,
# sortbycolumn, and sortbycolumnlist subcommands.
#
# Structure of the module:
#   - Public procedures related to sorting
#   - Private procedures implementing the sorting
#
# Copyright (c) 2000-2025  Csaba Nemethi (E-mail: csaba.nemethi@t-online.de)
#==============================================================================

#
# Public procedures related to sorting
# ====================================
#

#------------------------------------------------------------------------------
# tablelist::sortByColumn
#
# Sorts the content of the tablelist widget win by its col'th column.  Returns
# the sort order (increasing or decreasing).
#------------------------------------------------------------------------------
proc tablelist::sortByColumn {win col} {
    #
    # Check the arguments
    #
    if {![winfo exists $win]} {
	return -code error "bad window path name \"$win\""
    }
    if {[winfo class $win] ne "Tablelist"} {
	return -code error "window \"$win\" is not a tablelist widget"
    }
    if {[catch {::$win columnindex $col} result] != 0} {
	return -code error $result
    }
    if {$result < 0 || $result >= [::$win columncount]} {
	return -code error "column index \"$col\" out of range"
    }
    set col $result
    if {[::$win columncget $col -showlinenumbers]} {
	return ""
    }

    #
    # Determine the sort order
    #
    if {[set idx [lsearch -exact [::$win sortcolumnlist] $col]] >= 0 &&
	[lindex [::$win sortorderlist] $idx] eq "increasing"} {
	set sortOrder decreasing
    } else {
	set sortOrder increasing
    }

    #
    # Sort the widget's content based on the given column
    #
    if {[catch {::$win sortbycolumn $col -$sortOrder} result] == 0} {
	set userData [list $col $sortOrder]
	genVirtualEvent $win <<TablelistColumnSorted>> $userData

	return $sortOrder
    } else {
	return -code error $result
    }
}

#------------------------------------------------------------------------------
# tablelist::addToSortColumns
#
# Adds the col'th column of the tablelist widget win to the latter's list of
# sort columns and sorts the content of the widget by the modified column
# list.  Returns the specified column's sort order (increasing or decreasing).
#------------------------------------------------------------------------------
proc tablelist::addToSortColumns {win col} {
    #
    # Check the arguments
    #
    if {![winfo exists $win]} {
	return -code error "bad window path name \"$win\""
    }
    if {[winfo class $win] ne "Tablelist"} {
	return -code error "window \"$win\" is not a tablelist widget"
    }
    if {[catch {::$win columnindex $col} result] != 0} {
	return -code error $result
    }
    if {$result < 0 || $result >= [::$win columncount]} {
	return -code error "column index \"$col\" out of range"
    }
    set col $result
    if {[::$win columncget $col -showlinenumbers]} {
	return ""
    }

    #
    # Update the lists of sort columns and orders
    #
    set sortColList [::$win sortcolumnlist]
    set sortOrderList [::$win sortorderlist]
    if {[set idx [lsearch -exact $sortColList $col]] >= 0} {
	if {[lindex $sortOrderList $idx] eq "increasing"} {
	    set sortOrder decreasing
	} else {
	    set sortOrder increasing
	}
	set sortOrderList [lreplace $sortOrderList $idx $idx $sortOrder]
    } else {
	lappend sortColList $col
	lappend sortOrderList increasing
	set sortOrder increasing
    }

    #
    # Sort the widget's content according to the
    # modified lists of sort columns and orders
    #
    if {[catch {::$win sortbycolumnlist $sortColList $sortOrderList} result]
	== 0} {
	set userData [list $sortColList $sortOrderList]
	genVirtualEvent $win <<TablelistColumnsSorted>> $userData

	return $sortOrder
    } else {
	return -code error $result
    }
}

#
# Private procedures implementing the sorting
# ===========================================
#

#------------------------------------------------------------------------------
# tablelist::sortItems
#
# Processes the tablelist sort, sortbycolumn, and sortbycolumnlist subcommands.
#------------------------------------------------------------------------------
proc tablelist::sortItems {win parentKey sortColList sortOrderList} {
    variable snipSides
    upvar ::tablelist::ns${win}::data data

    set sortAllItems [expr {$parentKey eq "root"}]

    #
    # Make sure sortOrderList has the same length as sortColList
    #
    set sortColCount [llength $sortColList]
    set sortOrderCount [llength $sortOrderList]
    if {$sortOrderCount < $sortColCount} {
	for {set n $sortOrderCount} {$n < $sortColCount} {incr n} {
	    lappend sortOrderList increasing
	}
    } else {
	set sortOrderList [lrange $sortOrderList 0 [expr {$sortColCount - 1}]]
    }

    #
    # Save the keys corresponding to anchorRow and activeRow
    #
    foreach type {anchor active} {
	set ${type}Key [lindex $data(keyList) $data(${type}Row)]
    }

    #
    # Save some data of the edit window if present
    #
    if {[set editCol $data(editCol)] >= 0} {
	set editKey $data(editKey)
	saveEditData $win
    }

    #
    # Update the sort info and sort the item list
    #
    set descItemList {}
    if {[llength $sortColList] == 1 && [lindex $sortColList 0] == -1} {
	if {$data(-sortcommand) eq ""} {
	    return -code error "value of the -sortcommand option is empty"
	}

	set order [lindex $sortOrderList 0]

	if {$sortAllItems} {
	    #
	    # Update the sort info
	    #
	    for {set col 0} {$col < $data(colCount)} {incr col} {
		set data($col-sortRank) 0
		set data($col-sortOrder) ""
	    }
	    set data(sortColList) {}
	    set data(arrowColList) {}
	    set data(sortOrder) $order
	}

	#
	# Sort the child item list
	#
	sortChildren $win $parentKey [list lsort -$order -command \
	    $data(-sortcommand)] descItemList
    } else {					;# sorting by a column (list)
	#
	# Check the specified column indices
	#
	set sortColCount2 $sortColCount
	foreach col $sortColList {
	    if {$data($col-showlinenumbers)} {
		incr sortColCount2 -1
	    }
	}
	if {$sortColCount2 == 0} {
	    return ""
	}

	if {$sortAllItems} {
	    #
	    # Update the sort info
	    #
	    for {set col 0} {$col < $data(colCount)} {incr col} {
		set data($col-sortRank) 0
		set data($col-sortOrder) ""
	    }
	    set rank 1
	    foreach col $sortColList order $sortOrderList {
		if {$data($col-showlinenumbers)} {
		    continue
		}

		set data($col-sortRank) $rank
		set data($col-sortOrder) $order
		incr rank
	    }
	    makeSortAndArrowColLists $win
	}

	#
	# Sort the child item list based on the specified columns
	#
	for {set idx [expr {$sortColCount - 1}]} {$idx >= 0} {incr idx -1} {
	    set col [lindex $sortColList $idx]
	    if {$data($col-showlinenumbers)} {
		continue
	    }

	    set descItemList {}
	    set order [lindex $sortOrderList $idx]
	    if {$data($col-sortmode) eq "command"} {
		if {![info exists data($col-sortcommand)]} {
		    return -code error "value of the -sortcommand option for\
					column $col is missing or empty"
		}

		sortChildren $win $parentKey [list lsort -$order -index $col \
		    -command $data($col-sortcommand)] descItemList
	    } elseif {$data($col-sortmode) eq "asciinocase"} {
		if {$::tcl_version >= 8.5} {
		    sortChildren $win $parentKey [list lsort -$order \
			-index $col -ascii -nocase] descItemList
		} else {
		    sortChildren $win $parentKey [list lsort -$order \
			-index $col -command compareNoCase] descItemList
		}
	    } else {
		sortChildren $win $parentKey [list lsort -$order -index $col \
		    -$data($col-sortmode)] descItemList
	    }
	}
    }

    if {$sortAllItems} {
	#
	# Cancel the execution of all delayed
	# redisplay and redisplayCol commands
	#
	foreach name [array names data *redispId] {
	    after cancel $data($name)
	    unset data($name)
	}

	set canvasWidth $data(arrowWidth)
	if {[llength $data(arrowColList)] > 1} {
	    variable scalingpct
	    if {$scalingpct > 150} {
		variable centerArrows
		incr canvasWidth [expr {$centerArrows ? 7 : 9}]
	    } else {
		incr canvasWidth 6
	    }
	}
	foreach col $data(arrowColList) {
	    #
	    # Make sure the arrow will fit into the column
	    #
	    set idx [expr {2*$col}]
	    set pixels [lindex $data(colList) $idx]
	    if {$pixels == 0 && $data($col-maxPixels) > 0 &&
		$data($col-reqPixels) > $data($col-maxPixels) &&
		$data($col-maxPixels) < $canvasWidth} {
		set data($col-maxPixels) $canvasWidth
		set data($col-maxwidth) -$canvasWidth
	    }
	    if {$pixels != 0 && $pixels < $canvasWidth} {
		set data(colList) \
		    [lreplace $data(colList) $idx $idx $canvasWidth]
		set idx [expr {3*$col}]
		set data(-columns) \
		    [lreplace $data(-columns) $idx $idx -$canvasWidth]
	    }
	}

	#
	# Adjust the columns; this will also place the
	# canvas widgets into the corresponding labels
	#
	adjustColumns $win allLabels 1
    }

    if {[llength $descItemList] == 0} {
	return ""
    }

    set parentRow [keyToRow $win $parentKey]
    set firstDescRow [expr {$parentRow + 1}]
    set lastDescRow [expr {$parentRow + [descCount $win $parentKey]}]
    set firstDescLine [expr {$firstDescRow + 1}]
    set lastDescLine [expr {$lastDescRow + 1}]

    #
    # Update the line numbers (if any)
    #
    for {set col 0} {$col < $data(colCount)} {incr col} {
	if {!$data($col-showlinenumbers)} {
	    continue
	}

	set newDescItemList {}
	set line $firstDescLine
	foreach item $descItemList {
	    set item [lreplace $item $col $col $line]
	    lappend newDescItemList $item
	    set key [lindex $item end]
	    if {![info exists data($key-hide)]} {
		incr line
	    }
	}
	set descItemList $newDescItemList
    }

    set data(itemList) [eval [list lreplace $data(itemList) \
	$firstDescRow $lastDescRow] $descItemList]

    #
    # Replace the content of the list variable if present
    #
    condUpdateListVar $win

    #
    # Remove the tags elidedRow and hiddenRow from all lines between
    # 1 and $lastDescLine.  For the lines between $firstDescLine and
    # $lastDescLine this is needed because of the sorting, and for the
    # others because it improves the performance quite significantly.
    #
    variable pu
    set w $data(body)
    $w tag remove elidedRow 1.0 $lastDescLine.end+1$pu
    $w tag remove hiddenRow 1.0 $lastDescLine.end+1$pu

    #
    # Delete the items from the body text widget and insert the sorted ones.
    # Interestingly, for a large number of items it is much more efficient
    # to empty each line individually than to invoke a global delete command.
    #
    for {set line $firstDescLine} {$line <= $lastDescLine} {incr line} {
	$w delete $line.0 $line.end
    }
    set snipStr $data(-snipstring)
    set rowTagRefCount $data(rowTagRefCount)
    set cellTagRefCount $data(cellTagRefCount)
    set isSimple [expr {$data(imgCount) == 0 && $data(winCount) == 0 &&
			$data(indentCount) == 0}]
    set padY [expr {[$w cget -spacing1] == 0}]
    set descKeyList {}
    for {set row $firstDescRow; set line $firstDescLine} \
	{$row <= $lastDescRow} {set row $line; incr line} {
	set item [lindex $data(itemList) $row]
	set key [lindex $item end]
	lappend descKeyList $key
	set data($key-row) $row
	set dispItem [lrange $item 0 $data(lastCol)]
	if {$data(hasFmtCmds)} {
	    set dispItem [formatItem $win $key $row $dispItem]
	}
	if {[string match "*\t*" $dispItem]} {
	    set dispItem [mapTabs $dispItem]
	}

	#
	# Clip the elements if necessary and
	# insert them with the corresponding tags
	#
	if {$rowTagRefCount == 0} {
	    set hasRowFont 0
	} else {
	    set hasRowFont [info exists data($key-font)]
	}
	set col 0
	if {$isSimple} {
	    set insertArgs {}
	    set multilineData {}
	    foreach text $dispItem \
		    colFont $data(colFontList) \
		    colTags $data(colTagsList) \
		    {pixels alignment} $data(colList) {
		#
		# Build the list of tags to be applied to the cell
		#
		if {$hasRowFont} {
		    set cellFont $data($key-font)
		} else {
		    set cellFont $colFont
		}
		set cellTags $colTags
		if {$cellTagRefCount != 0} {
		    if {[info exists data($key,$col-font)]} {
			set cellFont $data($key,$col-font)
			lappend cellTags cell-font-$data($key,$col-font)
		    }
		}

		#
		# Clip the element if necessary
		#
		set multiline [string match "*\n*" $text]
		if {$pixels == 0} {		;# convention: dynamic width
		    if {$data($col-maxPixels) > 0} {
			if {$data($col-reqPixels) > $data($col-maxPixels)} {
			    set pixels $data($col-maxPixels)
			}
		    }
		}
		if {$pixels != 0} {
		    incr pixels $data($col-delta)

		    if {$data($col-wrap) && !$multiline} {
			if {[font measure $cellFont -displayof $win $text] >
			    $pixels} {
			    set multiline 1
			}
		    }

		    set snipSide \
			$snipSides($alignment,$data($col-changesnipside))
		    if {$multiline} {
			set list [split $text "\n"]
			if {$data($col-wrap)} {
			    set snipSide ""
			}
			set text [joinList $win $list $cellFont \
				  $pixels $snipSide $snipStr]
		    } elseif {!$data(-displayondemand)} {
			set text [strRange $win $text $cellFont \
				  $pixels $snipSide $snipStr]
		    }
		}

		if {$multiline} {
		    lappend insertArgs "\t\t" $cellTags
		    lappend multilineData \
			    $col $text $cellFont $pixels $alignment
		} elseif {$data(-displayondemand)} {
		    lappend insertArgs "\t\t" $cellTags
		} else {
		    lappend insertArgs "\t$text\t" $cellTags
		}

		incr col
	    }

	    #
	    # Insert the item into the body text widget
	    #
	    if {[llength $insertArgs] != 0} {
		eval [list $w insert $line.0] $insertArgs
	    }

	    #
	    # Embed the message widgets displaying multiline elements
	    #
	    foreach {col text font pixels alignment} $multilineData {
		findTabs $win $w $line $col $col tabIdx1 tabIdx2
		set msgScript [list ::tablelist::displayText $win $key \
			       $col $text $font $pixels $alignment]
		set msgScript [string map {"%" "%%"} $msgScript]
		$w window create $tabIdx2 -pady $padY -create $msgScript
		$w tag add elidedWin $tabIdx2
	    }

	} else {
	    foreach text $dispItem \
		    colFont $data(colFontList) \
		    colTags $data(colTagsList) \
		    {pixels alignment} $data(colList) {
		#
		# Build the list of tags to be applied to the cell
		#
		if {$hasRowFont} {
		    set cellFont $data($key-font)
		} else {
		    set cellFont $colFont
		}
		set cellTags $colTags
		if {$cellTagRefCount != 0} {
		    if {[info exists data($key,$col-font)]} {
			set cellFont $data($key,$col-font)
			lappend cellTags cell-font-$data($key,$col-font)
		    }
		}

		#
		# Insert the text and the label or window
		# (if any) into the body text widget
		#
		appendComplexElem $win $key $row $col $text $pixels \
				  $alignment $snipStr $cellFont $cellTags $line

		incr col
	    }
	}

	if {$rowTagRefCount != 0} {
	    if {[info exists data($key-font)]} {
		$w tag add row-font-$data($key-font) $line.0 $line.end
	    }
	}

	#
	# Restore the tags elidedRow and hiddenRow for this row if needed
	#
	if {[info exists data($key-elide)]} {
	    $w tag add elidedRow $line.0 $line.end+1$pu
	}
	if {[info exists data($key-hide)]} {
	    $w tag add hiddenRow $line.0 $line.end+1$pu
	}
    }

    #
    # Restore the tags elidedRow and hiddenRow
    # for the rows between 0 and $parentRow, too
    #
    for {set row 0; set line 1} {$row <= $parentRow} \
	{set row $line; incr line} {
	set item [lindex $data(itemList) $row]
	set key [lindex $item end]
	if {[info exists data($key-elide)]} {
	    $w tag add elidedRow $line.0 $line.end+1$pu
	}
	if {[info exists data($key-hide)]} {
	    $w tag add hiddenRow $line.0 $line.end+1$pu
	}
    }

    set data(keyList) [eval [list lreplace $data(keyList) \
	$firstDescRow $lastDescRow] $descKeyList]

    if {$sortAllItems} {
	#
	# Validate the key -> row mapping
	#
	set data(keyToRowMapValid) 1
	if {[info exists data(mapId)]} {
	    after cancel $data(mapId)
	    unset data(mapId)
	}
    }

    #
    # Invalidate the list of row indices indicating the viewable rows
    #
    set data(viewableRowList) {-1}

    #
    # Disable the body text widget if it was disabled before
    #
    if {$data(isDisabled)} {
	$w tag add disabled 1.0 end
	$w tag configure select -borderwidth 0
    }

    #
    # Update anchorRow and activeRow
    #
    foreach type {anchor active} {
	upvar 0 ${type}Key key2
	if {$key2 ne ""} {
	    set data(${type}Row) [keyToRow $win $key2]
	}
    }

    #
    # Bring the "most important" row into view if appropriate
    #
    if {$editCol >= 0} {
	set editRow [keyToRow $win $editKey]
	if {$editRow >= $firstDescRow && $editRow <= $lastDescRow} {
	    if {$::tk_version >= 8.5} {
		$w sync -command \
		    [list tablelist::doEditCell $win $editRow $editCol 1]
	    } else {
		doEditCell $win $editRow $editCol 1
	    }
	}
    } else {
	set selRows [curSelection $win]
	if {[llength $selRows] == 1} {
	    set selRow [lindex $selRows 0]
	    set selKey [lindex $data(keyList) $selRow]
	    if {$selRow >= $firstDescRow && $selRow <= $lastDescRow &&
		![info exists data($selKey-elide)]} {
		if {$::tk_version >= 8.5} {
		    $w sync -command [list tablelist::seeRow $win $selRow]
		} else {
		    seeRow $win $selRow
		}
	    }
	} elseif {[focus -lastfor $w] eq $w} {
	    set activeRow $data(activeRow)
	    set activeKey [lindex $data(keyList) $activeRow]
	    if {$activeRow >= $firstDescRow && $activeRow <= $lastDescRow &&
		![info exists data($activeKey-elide)]} {
		if {$::tk_version >= 8.5} {
		    $w sync -command [list tablelist::seeRow $win $activeRow]
		} else {
		    seeRow $win $activeRow
		}
	    }
	}
    }

    #
    # Adjust the elided text and restore the stripes in the body text widget
    #
    adjustElidedText $win
    redisplayVisibleItems $win
    makeStripes $win
    adjustSepsWhenIdle $win
    updateVScrlbarWhenIdle $win

    #
    # Work around a Tk bug on Mac OS X Aqua
    #
    variable winSys
    if {$winSys eq "aqua"} {
	foreach col $data(arrowColList) {
	    set canvas [list $data(hdrTxtFrmCanv)$col]
	    after idle [list lower $canvas]
	    after idle [list raise $canvas]
	}
    }

    return ""
}

#------------------------------------------------------------------------------
# tablelist::sortChildren
#
# Sorts the children of a given parent within the tablelist widget win,
# recursively.
#------------------------------------------------------------------------------
proc tablelist::sortChildren {win parentKey sortCmd itemListName} {
    upvar $itemListName itemList ::tablelist::ns${win}::data data

    set childKeyList $data($parentKey-childList)
    if {[llength $childKeyList] == 0} {
	return ""
    }

    #
    # Build and sort the list of child items
    #
    set childItemList {}
    foreach childKey $childKeyList {
	lappend childItemList [lindex $data(itemList) [keyToRow $win $childKey]]
    }
    set childItemList [eval $sortCmd [list $childItemList]]

    #
    # Update the lists and invoke the procedure recursively for the children
    #
    set data($parentKey-childList) {}
    foreach item $childItemList {
	lappend itemList $item
	set childKey [lindex $item end]
	lappend data($parentKey-childList) $childKey

	sortChildren $win $childKey $sortCmd itemList
    }
}

#------------------------------------------------------------------------------
# tablelist::sortList
#
# Sorts the specified list by the current sort columns of the tablelist widget
# win, using their current sort orders.
#------------------------------------------------------------------------------
proc tablelist::sortList {win list} {
    upvar ::tablelist::ns${win}::data data
    set sortColList $data(sortColList)
    set sortOrderList {}
    foreach col $sortColList {
	lappend sortOrderList $data($col-sortOrder)
    }

    if {[llength $sortColList] == 1 && [lindex $sortColList 0] == -1} {
	if {$data(-sortcommand) eq ""} {
	    return -code error "value of the -sortcommand option is empty"
	}

	#
	# Sort the list
	#
	set order [lindex $sortOrderList 0]
	return [lsort -$order -command $data(-sortcommand) $list]
    } else {
	#
	# Sort the list based on the specified columns
	#
	set sortColCount [llength $sortColList]
	for {set idx [expr {$sortColCount - 1}]} {$idx >= 0} {incr idx -1} {
	    set col [lindex $sortColList $idx]
	    set order [lindex $sortOrderList $idx]

	    if {$data($col-sortmode) eq "command"} {
		if {![info exists data($col-sortcommand)]} {
		    return -code error "value of the -sortcommand option for\
					column $col is missing or empty"
		}

		set list [lsort -$order -index $col -command \
			  $data($col-sortcommand) $list]
	    } elseif {$data($col-sortmode) eq "asciinocase"} {
		if {$::tcl_version >= 8.5} {
		    set list [lsort -$order -index $col -ascii -nocase $list]
		} else {
		    set list [lsort -$order -index $col -command \
			      compareNoCase $list]
		}
	    } else {
		set list [lsort -$order -index $col -$data($col-sortmode) $list]
	    }
	}

	return $list
    }
}

#------------------------------------------------------------------------------
# tablelist::compareNoCase
#
# Compares the given strings in a case-insensitive manner.
#------------------------------------------------------------------------------
proc tablelist::compareNoCase {str1 str2} {
    return [string compare [string tolower $str1] [string tolower $str2]]
}
